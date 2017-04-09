;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package:CL-WHY; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-why/specials.lisp,v 1.6 2009/01/26 11:10:49 edi Exp $

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

#+:sbcl
(defmacro defconstant (name value &optional doc)
  "Make sure VALUE is evaluated only once \(to appease SBCL)."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defstruct <raw>
  "A wrapper for objects to print without HTML escapes."
  (data nil :read-only t))

(defstruct <esc>
  "A wrapper for objects to print with HTML escapes."
  (data nil :read-only t))

(defun -raw- (data)
  "Create a wrapper of type <RAW> for DATA if it is non-null, otherwise return NIL."
  (when data (make-<raw> :data data)))

(defun -esc- (data)
  "Create a wrapper of type <ESC> for DATA if it is non-null, otherwise return NIL."
  (when data (make-<esc> :data data)))

(defmethod print-object ((object <raw>) stream)
  "Prints a <RAW> object into the STREAM."
  (cond ((or *print-readably* *print-escape*) (call-next-method))
        (t (princ (slot-value object 'data) stream))))

(defmethod print-object ((object <esc>) stream)
  "Prints a <ESC> object into the STREAM."
  (cond ((or *print-readably* *print-escape*) (call-next-method))
        (t (write-string (escape-string (princ-to-string (slot-value object 'data)))
            stream))))

(defun wrapper-p (object)
  "Returns whether object is a wrapper of either type <RAW> or <ESC>."
  (typecase object ((or <raw> <esc>) t)))

(defvar *prologue*
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
  "This is the first line that'll be printed if the :PROLOGUE keyword
argument is T")

(defvar *escape-char-p*
  (lambda (char)
    (or (find char "<>&'\"")
        (> (char-code char) 127)))
  "Used by ESCAPE-STRING to test whether a character should be escaped.")

(defvar *indent* nil
  "Whether to insert line breaks and indent. Also controls amount of
indentation dynamically.")

(defvar *html-mode* :xml
  ":SGML for \(SGML-)HTML, :XML \(default) for XHTML, :HTML5 for HTML5.")

(defvar *downcase-tokens-p* t
  "If NIL, a keyword symbol representing a tag or attribute name will
not be automatically converted to lowercase.  This is useful when one
needs to output case sensitive XML.")

(defvar *attribute-quote-char* #\'
  "Quote character for attributes.")

(defvar *empty-tag-end* " />"
  "End of an empty tag.  Default is XML style.")

(defvar *html-no-indent-tags*
  '(:pre :textarea)
  "The list of HTML tags that should disable indentation inside them. The initial
value is a list containing only :PRE and :TEXTAREA.")

(defvar *html-empty-tags*
  '(:area
    :atop
    :audioscope
    :base
    :basefont
    :br
    :choose
    :col
    :command
    :embed
    :frame
    :hr
    :img
    :input
    :isindex
    :keygen
    :left
    :limittext
    :link
    :meta
    :nextid
    :of
    :over
    :param
    :range
    :right
    :source
    :spacer
    :spot
    :tab
    :track
    :wbr)
  "The list of HTML tags that should be output as empty tags.
See *HTML-EMPTY-TAG-AWARE-P*.")

(defvar *html-empty-tag-aware-p* t
  "Set this to NIL to if you want to use CL-WHY as a strict XML
generator.  Otherwise, CL-WHY will only write empty tags listed
in *HTML-EMPTY-TAGS* as <tag/> \(XHTML mode) or <tag> \(SGML
mode and HTML5 mode).  For all other tags, it will always generate
<tag></tag>.")

(defconstant +newline+ (make-string 1 :initial-element #\Newline)
  "Used for indentation.")

(defconstant +spaces+ (make-string 2000
                                   :initial-element #\Space
                                   :element-type 'base-char)
  "Used for indentation.")

