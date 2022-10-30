;;;; Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>
;;;; 
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :too)

;;;; This file mimicks functionality of the library lisp-at-work due to TOO's license.
;;;;  The functions are typically custom-tailored to TOO's simpler requirements:
;;;;   Only a subset of the original requirements is considered.
;;;;   lisp-at-work's concepts are used informally.
;;;;   The functions are by no means as powerful as lisp-at-work's approach, but sufficient for the use case. For example, lisp-at-work's clean escaping solution is taken out from context here (escaping won't work anyway since any given primitive is directly rendered and since everything gets flattened aggressively).
;;;;  Largely remains interface-compatible (most functions are quick helpers after all).
;;;;  Interestingly, some shortcuts proved useful and made the way into lisp-at-work indeed.

;;Authority: lisp-at-work::flatten.
(defun flatten (tree-with-atoms-or-lists)
  "Given a (possibly nested) list with either atoms or lists with atoms, return a flat list of atoms.
   Won't dissolve law-primitives."
  (if (consp tree-with-atoms-or-lists)
      (loop for item in tree-with-atoms-or-lists
            append (if (consp item)
                       (flatten item)
                       (list item)))
      tree-with-atoms-or-lists))
(flatten (list 1 2 3 (list 4 (list 5.1 5.2) 6) 7 (list 8.1 8.2) 9))

;;Authority: Parenscript/src/printer.lisp.
(defun escape-js (object)
  (let ((escaped-chars (list
                        ;;Firefox is not happy with escaped single quotes in JSONs.
                        ;;#\' #\'
                        #\"            #\"
                        #\\            #\\
                        #\Backspace    #\b
                        (code-char 12) #\f
                        #\Newline      #\n
                        #\Return       #\r
                        #\Tab          #\t)))
    (with-output-to-string (out)
      (loop for char across object do
        (cond ((getf escaped-chars char)
               (princ #\\ out)
               (princ (getf escaped-chars char) out))
              ((or (<= (char-code char) #x1F)
                   (<= #x80 (char-code char) #x9F)
                   (member (char-code char) '(#xA0 #xAD #x200B #x200C)))
               (format out "\\u~:@(~4,'0x~)" (char-code char)))
              (T (princ char out)))))))

(defun escape-xml (text &optional (levels 1))
  "LEVELS is for entertainment purposes only.
   Also see #'present-for-schoolclass."
  (if (< 1 levels)
      (escape-xml (escape-xml text (1- levels)))
      (with-output-to-string (*standard-output*)
        (loop for char across text
              do (case char
                   (#\< (format T "&lt;"))
                   (#\> (format T "&gt;"))
                   (#\& (format T "&quot;"))
                   (T (write-char char)))))))
(escape-xml "<b>ABC & DEF</b>" 10)


;;; #'dsl

;;Attention (escaping): directly renders the primitive.
;;Authority: #'lisp-at-work:dsl.
(defgeneric dsl (sugar &rest body)
  (:documentation
   "dsl: domain-specific-language / direct-syntax-literally / distribute-sugar-loosely / Do-as-I-Say (and not as I (should) do) / ...
    Quick helper for lisp-at-work's law-dsl. A do-what-i-mean function.
    LAW does not strive for complete coverage of any language in order to be useful, nor does it necessarily strive for a representation of all imaginable languages.
    Instead, it supports fallback strings and a generic law-dsl which accepts formatting guidelines in the primitives themselves.
    This is a quick and incomplete approach, but very useful for drafts/exploratory programming.
    It is easy to quickly represent structured information in the CL editor this way.
    This function is intended for semi-structured information, or seamless movement towards structure, since more and more effort results in a more and more accurate AST representation with easily catchable patterns. Consider using lisp-at-work's parsers for serious porting/migration projects.
    Also see #'lisp-at-work:render-quick.
    
    SUGAR: see applicable methods.
    
    BODY: BODY gets #'flatten -ed, which allows for convenient ,@-like splicing of the &rest parameter BODY.
    
    Design goal: \"1NF\" for non-lisp code. Structured information is almost always easily mappable to symbolic expressions. Separate the syntax of composite elements from otherwise quite isolated constituent elements. Additionally, do not introduce much typing on the Lisp side (sparse use of keywords).")
  (:method ((sugar null) &rest body)
    "SUGAR leading to no prefix, no glue, no suffix. Functionally equivalent to the empty string."
    ;;Authority: TOO.
    (call-next-method "" body))
  (:method ((sugar (eql :quote)) &rest body)
    "Wrap in double quote: no glue and the prefix and suffix are each #\"."
    ;;Authority: TOO.
    (call-next-method "\"\"" body))
  (:method ((sugar (eql :squote)) &rest body)
    "Wrap in single quote: no glue and the prefix and suffix are each #\'."
    ;;Authority: TOO.
    (call-next-method "\'\'" body))
  ;;Authority: lisp-at-work.
  ;;TODO: use more of CL's type system.
  (:method ((sugar T) &rest body)
    "Case SUGAR:
      a character/string with one character. In this case, SUGAR represents the glue of the join/implode string operation.
      a list with one element. In this case, (first SUGAR) is the glue. It can consist of multiple characters this way.
      a string with three characters. In this case, the first character defines the opening symbol, the second the glue and the last the closing symbol.
      a list with three strings/characters. In this case, the prefix, glue and suffix all can consist of multiple characters.
      a sequence with two elements. In this case, there is no glue / the glue is the empty string.
      the empty string. No prefix, no glue, and no suffix."
    (let* ((sugar-sequence? (and (or (stringp sugar) (consp sugar))
                                 (< 1 (length sugar))))
           (prefix (when sugar-sequence? (elt sugar 0)))
           (glue (cond ((and sugar-sequence? (= 2 (length sugar))) "")
                       ((and sugar-sequence? (= 3 (length sugar))) (elt sugar 1))
                       ((consp sugar) (car sugar))
                       (T (unless (symbolp sugar) sugar))))
           (suffix (when sugar-sequence?
                     ;;Index 1 or 2.
                     (elt sugar (1- (length sugar))))))
      ;;Authority: TOO.
      ;;THINK: Is indentation possible at all when BODY is already a string? → Would need *indentation* or something.
      ;;        Introducing whitespace in a target language with significant whitespace possibly changes semantics/intention.
      ;;       Decision: Not intending due to unknown involved languages (and YAGNI).
      (format nil (format nil "~~<~@[~A~]~~;~~@{~~A~~^~@[~A~]~~}~~;~@[~A~]~~:>" prefix glue suffix)
              (remove nil (flatten body))))))
(dsl "{ }" "#'dsl allows for quick non-Lisp code" (dsl '(" and ") "navigation" "movement"))

(flet ((internal-funcall (function-name &rest arguments)
         (dsl (list (format nil "~A(" function-name) "," ")") arguments)))
  ;;Authority: lisp-at-work.
  (defun jsq-funcall (function-name &rest arguments)
    "Quick helper for js-funcall."
    ;;Authority: TOO.
    (apply #'internal-funcall function-name arguments))

  ;;Authority: lisp-at-work.
  (defun sqlq-call-function (function-name &rest arguments)
    "Quick helper for sql-call-function."
    ;;Authority: TOO.
    (apply #'internal-funcall function-name arguments)))
(jsq-funcall "abc" "def" "ghi")
(sqlq-call-function "abc" "def" "ghi")

(defun jsq-ternary (test then else)
  "Javascript's ternary operator."
  (dsl "( )" test "?" then ":" else))
(jsq-ternary "abc" "def" "ghi")

(defun sqlq-let* (inner-query additional-columns &optional (inner-column-selection ()) where)
  "ADDITIONAL-COLUMNS can access computed columns in INNER-QUERY.
   The list INNER-COLUMN-SELECTION overrides the default behaviour of passing through all columns of INNER-QUERY.
   HANDLE WITH CARE: lavishly using this quick helper may trouble the SQL optimizer."
  (dsl " "
       "SELECT" (dsl "," (or inner-column-selection "*") additional-columns)
       "FROM"
       (dsl '(" AS ")
            (dsl "()" inner-query)
            (format nil "~A" (gensym)))
       (when where
         (list "WHERE" where))))


;;; XML (HTML SVG)

;;Authority: TOO.
(defun register-xml-tag (name)
  "Register a quick helper which allows for easy entry of XML DOMs."
  (eval `(defun ,(read-from-string (format nil "x-~A" name)) (&rest attributes-plist-and-possibly-appended-content)
           ,(format
             nil
             "Render the XML tag ~A.
              Attribute key values pairs are defined by alternating keywords with values in front of the content.
              Since ATTRIBUTES-AND-POSSIBLY-APPENDED-CONTENT is flattened, it is possible to nest the attributes, for example due through the inclusion of a function call. Direct flat entry of the attribute pairs, on the other hand, profits from less typing.
              Removes nil from ATTRIBUTES-AND-POSSIBLY-APPENDED-CONTENT." name)
           (let* ((flat (remove nil (flatten attributes-plist-and-possibly-appended-content)))
                  (attributes (loop for (a . d) on flat by #'cddr
                                    while (keywordp a)
                                    collect (list (string-downcase (symbol-name a)) (car d))))
                  (body (loop for (a . d) on flat by #'cddr
                              unless (keywordp a)
                                return (cons a d)))
                  (start (dsl (list (format nil "<~A~@[ ~]" ,name attributes)
                                    " "
                                    (if body ">" " />"))
                              (loop for (k . v) in attributes
                                    collect (dsl '("" "=\"" "\"") k v)))))
             (if body
                 (dsl (list start
                            ""
                            (format nil "</~A>" ,name))
                      body)
                 start)))))
(loop for tag in (append
                  (list "a" "b" "br" "body" "cite" "code" "details" "div" "form" "frame" "frameset" "h1" "h2" "h3" "h4" "h5" "hr" "head" "html" "i" "img" "input" "label" "li" "ol" "p" "pre" "script" "span" "style" "summary" "table" "td" "th" "textarea" "title" "tr" "ul")
                  (list "circle" "path" "rect" "svg")
                  (list "qgsScales" "scale")
                  (list "GDAL_WMS" "Service" "ServerUrl" "Layer" "ZoomLevel" "DataWindow" "UpperLeftX" "UpperLeftY" "LowerRightX" "LowerRightY" "TileLevel" "TileCountX" "TileCountY" "YOrigin" "Projection" "BlockSizeX" "BlockSizeY" "BandsCount" "ZeroBlockOnServerException" "ZeroBlockHttpCodes" "UnsafeSSL"))
      do (register-xml-tag tag))
(x-p :style "abc" :id "def" "ghi" "jkl")
(x-p :style "abc" "ghi" "jkl")
(x-p "ghi" "jkl")
(x-img "")
(x-img)
(x-img :src "abc.jpg" :title "def")

