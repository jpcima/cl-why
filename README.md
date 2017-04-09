# cl-why
(X)HTML generation macros, fork of CL-WHO

## Reason behind CL-WHY

This project is intended as an improvement over CL-WHO, a Common Lisp library for HTML generation.

I develop this fork because while a tree DSL is an elegant way to construct HTML documents, I disagree with some ideas of the original specification.

One particular problem with CL-WHO is about *escaping*: values are not escaped unless it is explicitly demanded.
On the contrary, the values should be written escaped by default and raw by exception; it reduces the probability of making mistakes and helps to ensure creating well-formed documents.

## Installing

Download CL-WHY in a path recognized by Quicklisp and execute the following command.

    (ql:quickload '#:cl-why)

## Differences

The usage of CL-WHY is largely similar to CL-WHO whose documentation is located [here](http://weitz.de/cl-who/).

Instead of a complete documentation I make a list of differences between these two libraries.

- Automatic escaping of attribute values and string nodes

      (with-html-output-to-string (s) (:a :href "a\"test" "foo<>bar"))
      CL-WHY: "<a href='a&quot;test'>foo&lt;&gt;bar</a>"
      CL-WHO: "<a href='a\"test'>foo<>bar</a>"

- Possibility to include raw strings, to use carefully

      (with-html-output-to-string (s) (:div (raw "<span>Hello</span>")))
      CL-WHY: "<div><span>Hello</span></div>"

- Accepted non-null values other than string at child node context (CL-WHO silently ignores these)

      (with-html-output-to-string (s) (:body 123 "abc" 456))
      CL-WHY: "<body>123abc456</body>"
      CL-WHO: "<body>abc</body>"

- Removed macros *str* and *fmt*, found no longer useful or redundant
