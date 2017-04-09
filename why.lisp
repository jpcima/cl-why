;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package:CL-WHY; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-why/why.lisp,v 1.42 2009/01/26 11:10:49 edi Exp $

;;; Copyright (c) 2003-2009, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-why)

(defun html-mode ()
  "Returns the current HTML mode. :SGML for \(SGML-)HTML, :XML for
XHTML and :HTML5 for HTML5 (HTML syntax)."
  *html-mode*)

(defun (setf html-mode) (mode)
  "Sets the output mode to XHTML or \(SGML-)HTML.  MODE can be
:SGML for HTML, :XML for XHTML or :HTML5 for HTML5 (HTML syntax)."
  (ecase mode
    ((:sgml)
     (setf *html-mode* :sgml
           *empty-tag-end* ">"
           *prologue* "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"))
    ((:xml)
     (setf *html-mode* :xml
           *empty-tag-end* " />"
           *prologue* "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"))
    ((:html5)
     (setf *html-mode* :html5
           *empty-tag-end* ">"
           *prologue* "<!DOCTYPE html>"))))

(defun process-tag (sexp body-fn)
  (declare (optimize speed space))
  "Returns a string list corresponding to the `HTML' \(in CL-WHY
syntax) in SEXP.  Uses the generic function CONVERT-TO-STRING-LIST
internally.  Utility function used by TREE-TO-TEMPLATE."
  (let (tag attr-list body)
    (cond
      ((keywordp sexp)
       (setq tag sexp))
      ((atom (first sexp))
       (setq tag (first sexp))
       ;; collect attribute/value pairs into ATTR-LIST and tag body (if
       ;; any) into BODY
       (loop for rest on (cdr sexp) by #'cddr
             if (keywordp (first rest))
               collect (cons (first rest) (second rest)) into attr
             else
               do (progn (setq attr-list attr)
                         (setq body rest)
                         (return))
             finally (setq attr-list attr)))
      ((listp (first sexp))
       (setq tag (first (first sexp)))
       (loop for rest on (cdr (first sexp)) by #'cddr
             if (keywordp (first rest))
               collect (cons (first rest) (second rest)) into attr
             finally (setq attr-list attr))
       (setq body (cdr sexp))))
    (convert-tag-to-string-list tag attr-list body body-fn)))

(defun convert-attributes (attr-list)
  "Helper function for CONVERT-TAG-TO-STRING-LIST which converts the
alist ATTR-LIST of attributes into a list of strings and/or Lisp
forms."
  (declare (optimize speed space))
  (loop with =var= = (gensym)
        for (orig-attr . val) in attr-list
        for attr = (if *downcase-tokens-p*
                     (string-downcase orig-attr)
                     (string orig-attr))
        unless (null val) ;; no attribute at all if VAL is NIL
          if (constantp val)
            if (and (eq *html-mode* :sgml) (eq val t)) ; special case for SGML
              nconc (list (-raw- #\ ) (-raw- attr))
            else
              nconc (list (-raw- #\ )
                          ;; name of attribute
                          (-raw- attr)
                          (-raw- #\=)
                          (-raw- *attribute-quote-char*)
                          ;; value of attribute
                          (cond ((eq val t)
                                 ;; VAL is T, use attribute's name
                                 (-raw- attr))
                                (t
                                 ;; constant form, PRINC it -
                                 ;; EVAL is OK here because of CONSTANTP
                                 (-esc- (eval val))))
                          (-raw- *attribute-quote-char*))
            end
          else
            ;; do the same things as above but at runtime
            nconc (list `(prog1 nil
                           (let ((,=var= ,val))
                             (cond ((null ,=var=))
                                   ((eq ,=var= t)
                                    ,(case *html-mode*
                                       (:sgml
                                        `(progn (raw #\ ) (raw ,attr)))
                                       ;; otherwise default to :xml mode
                                       (t
                                        `(progn
                                           (raw #\ ) (raw ,attr) (raw #\=)
                                           (raw *attribute-quote-char*)
                                           (raw ,attr)
                                           (raw *attribute-quote-char*)))))
                                   (t
                                    (raw #\ ) (raw ,attr) (raw #\=)
                                    (raw *attribute-quote-char*)
                                    (str ,=var=)
                                    (raw *attribute-quote-char*))))))))

(defgeneric convert-tag-to-string-list (tag attr-list body body-fn)
  (:documentation "Used by PROCESS-TAG to convert `HTML' into a list
of strings.  TAG is a keyword symbol naming the outer tag, ATTR-LIST
is an alist of its attributes \(the car is the attribute's name as a
keyword, the cdr is its value), BODY is the tag's body, and BODY-FN is
a function which should be applied to BODY.  The function must return
a list of strings or Lisp forms."))

(defmethod convert-tag-to-string-list (tag attr-list body body-fn)
  "The standard method which is not specialized.  The idea is that you
can use EQL specializers on the first argument."
  (declare (optimize speed space))
  (let ((tag (if *downcase-tokens-p* (string-downcase tag) (string tag)))
        (body-indent
          ;; increase *INDENT* by 2 for body -- or disable it
          (when (and *indent* (not (member tag *html-no-indent-tags* :test #'string-equal)))
            (+ 2 *indent*))))
    (nconc
     (if *indent*
       ;; indent by *INDENT* spaces
       (list (-raw- +newline+) (-raw- (n-spaces *indent*))))
     ;; tag name
     (list (-raw- #\<) (-raw- tag))
     ;; attributes
     (convert-attributes attr-list)
     ;; body
     (if body
       (append
        (list (-raw- #\>))
        ;; now hand over the tag's body to TREE-TO-TEMPLATE
        (let ((*indent* body-indent))
          (funcall body-fn body))
        (when body-indent
          ;; indentation
          (list (-raw- +newline+) (-raw- (n-spaces *indent*))))
        ;; closing tag
        (list (-raw- "</") (-raw- tag) (-raw- #\>)))
       ;; no body, so no closing tag unless defined in *HTML-EMPTY-TAGS*
       (if (or (not *html-empty-tag-aware-p*)
               (member tag *html-empty-tags* :test #'string-equal))
         (list (-raw- *empty-tag-end*))
         (list (-raw- #\>) (-raw- "</") (-raw- tag) (-raw- #\>)))))))

(defun tree-to-template (tree)
  "Transforms an HTML tree into an intermediate format - mainly a
flattened list of strings. Utility function used by TREE-TO-COMMANDS-AUX."
  (loop for element in tree
        if (or (keywordp element)
                 (and (listp element)
                      (keywordp (first element)))
                 (and (listp element)
                      (listp (first element))
                      (keywordp (first (first element)))))
        ;; the syntax for a tag - process it
        nconc (process-tag element #'tree-to-template)
        ;; list - insert as sexp
        else if (consp element)
        collect `(let ((*indent* ,*indent*))
                   nil ;; If the element is (declare ...) it
                       ;; won't be interpreted as a declaration and an
                       ;; appropriate error could be signaled
                   ,element)
        ;; something else - insert verbatim
        else
        collect element))

(defun list-to-string (list)
  (declare (optimize speed space))
  "Concatenates a list of objects into one string."
  (with-output-to-string (s)
    (dolist (object list)
      (princ object s))))

(defun conc (&rest list)
  "Concatenates all arguments into one string."
  (funcall #'list-to-string list))

(defun tree-to-commands (tree stream &key prologue ((:indent *indent*) *indent*))
  (declare (optimize speed space))
  (when (and *indent*
             (not (integerp *indent*)))
    (setq *indent* 0))
  (let ((in-const-p t)
        collector
        const-collector
        (template (tree-to-template tree)))
    (when prologue
      (push +newline+ template)
      (when (eq prologue t)
        (setq prologue *prologue*))
      (push (-raw- prologue) template))
    (flet ((emit-const-collector ()
             "Generate a WRITE-STRING statement for what is currently
in CONST-COLLECTOR."
             (list 'write-string (list-to-string (nreverse const-collector))
                   stream)))
      (loop for element in template if element
         do (let ((printable-constant
                   (cond ((wrapper-p element) element)
                         ((constantp element) (-esc- (eval element))))))
              (cond ((and in-const-p printable-constant)
                     ;; this element is a constant and the last one
                     ;; also was (or this is the first element) -
                     ;; collect into CONST-COLLECTOR
                     (push printable-constant const-collector))
                    (printable-constant
                     ;; the last one wasn't a constant so we start
                     ;; with an empty CONST-COLLECTOR
                     (setq const-collector (list printable-constant)
                           in-const-p t))
                    (t
                     ;; not a constant
                     (when const-collector
                       ;; CONST-COLLECTOR isn't empty so we have to emit the
                       ;; collected constants first
                       (push (emit-const-collector) collector)
                       (setq in-const-p nil
                             const-collector '()))
                     (push (list 'str element) collector)))))
      (if const-collector
        ;; finally empty CONST-COLLECTOR if there's something in it
        (nreverse (cons (emit-const-collector)
                        collector))
        (nreverse collector)))))

(defmacro with-html-output ((var &optional stream
                                 &rest rest
                                 &key prologue indent)
                            &body body)
  "Transform the enclosed BODY consisting of HTML as s-expressions
into Lisp code to write the corresponding HTML as strings to VAR -
which should either hold a stream or which'll be bound to STREAM if
supplied."
  (declare (ignore prologue))
  (multiple-value-bind (declarations forms) (extract-declarations body)
  `(let ((,var ,(or stream var)))
       ,@declarations
     (check-type ,var stream)
     (macrolet ((htm (&body body)
                  `(prog1 nil
                     (with-html-output (,',var nil :prologue nil :indent ,,indent)
                       ,@body)))
                (str (thing)
                  (with-unique-names (result)
                    `(prog1 nil
                       (let ((,result ,thing))
                         (when ,result
                           (write-string
                            (escape-string (princ-to-string ,result)) ,',var))))))
                (raw (thing)
                  (with-unique-names (result)
                    `(prog1 nil
                       (let ((,result ,thing))
                         (when ,result (princ ,result ,',var)))))))
         ,@(apply 'tree-to-commands forms var rest)))))

(defmacro with-html-output-to-string ((var &optional string-form
                                           &key #-(or :cmu)
                                                (element-type #-:lispworks ''character
                                                              #+:lispworks ''lw:simple-char)
                                                prologue
                                                indent)
                                      &body body)
  "Transform the enclosed BODY consisting of HTML as s-expressions
into Lisp code which creates the corresponding HTML as a string."
  (multiple-value-bind (declarations forms) (extract-declarations body)
  `(with-output-to-string (,var ,string-form
                                #-(or :cmu) :element-type
                                #-(or :cmu) ,element-type)
     ,@declarations
    (with-html-output (,var nil :prologue ,prologue :indent ,indent)
      ,@forms))))
