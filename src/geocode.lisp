;;;; TOO (The Offline Oriented) creates ZIM files with offline available maps
;;;; Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>
;;;; 
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :too)

#|
This file introduces geocoding capabilities to the ZIM file, i.e. enables users to search for place names that get resolved to coordinates somehow.
The solution must work with the prominent search bar that is promoted by the ZIM file readers.
TOO creates HTML files for each place.
 Redirects are not bookmarkable by ZIM readers.
The HTML files have to be small, which is why they only include a file that will include a map file (through #'place-file-builder).
The referenced map is only a base map without layers.
 As a consequence, a stateful link to the layer selection is included.
The place files are only intended for location discovery.

#'locator-payload-encode contains the explanation how points and MBRs are encoded in the place files.
|#

;;;; Explain how geocoding works in TOO.

(defun wiki-list (articles)
  "An HTML list with links to wiki articles."
  (when articles
    (x-ul (loop for def in (loop for key in articles
                                 collect (gethash key *wiki-definitions*))
                collect (x-li (x-a :href (format nil "wiki/~A" (url (wiki-article-file def)))
                                   (format nil "Small Wiki: ~A" (name def))))))))

(defun name-lookup-file (flavour)
  "name-lookup.html's content."
  (dir-entry
   :url "name-lookup.html"
   :title "Name lookup"
   :mimetype-id :html
   :content (dsl '("<!DOCTYPE html>" nil nil)
                 (x-html
                  (x-head (x-title "Name lookup")
                          *zim-style*
                          "<meta charset=\"utf-8\">"
                          "<style>td { text-align:left !important }body{margin:0em}</style>"
                          ;;Work around kiwix-android's missing form support.
                          (x-script :type "text/javascript"
                                    (dsl ";"
                                         (dsl "="
                                              "window.onsubmit"
                                              (dsl '("function (e) {" ";" "}")
                                                   ;;Keep kiwix-serve's suggestion box working.
                                                   (dsl '("if(e.target.elements['layer-selection']!==undefined){" ";" "}")
                                                        "e.preventDefault()"
                                                        ;;Authority: #'place-file-builder.
                                                        "myframe=document.createElement('iframe')"
                                                        "myframe.setAttribute('style','border-width:0;width:100%;height:100%;padding:0em;position:fixed;top:0em')"
                                                        (dsl '("myframe.setAttribute('src'," "" ")")
                                                             (dsl "+"
                                                                  "'layer-selection/'"
                                                                  "e.target.elements['layer-selection'].value"
                                                                  "'.html?filter='"
                                                                  "e.target.elements['filter'].value"))
                                                        "document.body.appendChild(myframe)"
                                                        "document.body.style.margin='0em'"
                                                        "document.body.style.overflow='hidden'"))))))
                  (x-body (background 0)
                          (back-to-welcome-page)
                          (x-h2 "Name lookup")
                          (x-h3 "Layer features")
                          (x-p "You can display layer features matching a search term only.")
                          (x-table (x-tr (x-th "Layer") (x-th "Search in the layer"))
                                   (loop for i = 0 then (incf i)
                                         for layer in (layers flavour)
                                         collect (x-tr (x-td (layer-name-0nf layer flavour T))
                                                       (x-td (x-form (x-input :type "hidden" :name "layer-selection" :value (toggle-layer-on i (layers flavour)))
                                                                     (x-input :type "hidden" :name "layer-name" :value (name layer))
                                                                     (x-input :name "filter")
                                                                     (x-input :type "submit" :value (format nil "Search in layer &quot;~A&quot;" (user-facing-name layer))))))))
                          (x-p "<b>Hint:</b> As the filter works on the whole tooltip content, you can also search for certain groups within a layer (if applicable).")
                          (x-h3 "Map labels")
                          (x-p "A lot of map labels are included in the search index - just use the search function of the ZIM browser. For each entry that is finally added to the search index, the English translation is also fetched from the data sources and added to the search index if it is available. Note that the ZIM file region is also applied to the search index: only intersecting geometries are included.")
                          (x-p "Each area belonging to a place with multiple disconnected areas is included as a separate search index entry - but only if it intersects the file region. The biggest intersecting area is called A1, the second-biggest intersecting area A2, etc.")
                          (x-p "Name collisions within the same search entry class or place type may occur. They are disambiguated with the suffixes D1 (disambiguation 1), D2 (disambiguation 2), etc. The rank is determined by the population size stored in the entries in this case, bigger populations being ranked higher. A default population is assumed for entries without available population information (cities: 100000, towns: 1000, 1 otherwise). If there still are ties amongst the entries, the entry with more registered metadata information pieces is ranked first. If all those mentioned values are <i>still</i> identical indeed, then the technical identification number is considered as the final arbiter, smaller technical identification numbers being ranked higher.")
                          (x-p "A table of included search entry classes follows.")
                          (x-table (x-tr (x-th "Names of") (x-th "Description") (x-th "Geometry type"))
                                   (x-tr (x-td "Countries")
                                         (x-td "<a name='ukraine'></a>The names of the countries of the world."
                                               (x-p "Recognition of countries and their borders is an international matter, and the United Nations in general and its Charter in particular are referred to quite often in the discourse.")
                                               (x-p "<i>Ad fontes</i> / To the sources:")
                                               (x-ul (x-li "The <a href='un/uncharter.html'>Charter of the United Nations</a>.")
                                                     (x-li "UN Resolution A/RES/68/262: <a href='un/A_RES_68_262-EN.html'>Territorial integrity of Ukraine</a>.")
                                                     (x-li "UN Resolution A/RES/ES-11/1: <a href='un/A_RES_ES-11_1-EN.html'>Aggression against Ukraine</a>.")
                                                     (x-li "UN Resolution A/RES/ES-11/2: <a href='un/A_RES_ES-11_2-EN.html'>Humanitarian consequences of the aggression against Ukraine</a>.")
                                                     (x-li "UN Resolution A/RES/ES-11/3: <a href='un/A_RES_ES-11_3-EN.html'>Suspension of the rights of membership of the Russian Federation in the Human Rights Council</a>.")
                                                     (x-li "UN Resolution A/RES/ES-11/4: <a href='un/A_RES_ES-11_4-EN.html'>Territorial integrity of Ukraine: defending the principles of the Charter of the United Nations</a>.")))
                                         (x-td "Polygon"))
                                   (x-tr (x-td "Capitals of countries")
                                         (x-td "The names of the capitals of countries. The value of the place type, if available, is appended for clarity. For example, Funafuti is listed as &quot;Funafuti (capital municipality)&quot;.")
                                         (x-td "Point"))
                                   (x-tr (x-td "Administrative regions")
                                         (x-td "Governments hierarchically divide their territory into administrative regions. Since the names for those regions vary from country to country, TOO simply uses 3 for the regions highest in the hierarchy and counts upwards as the hierarchy gets deeper. The search entries may still contain the name of such regions in their names itself.")
                                         (x-td "Polygon"))
                                   (x-tr (x-td "Cities, towns, villages and smaller places")
                                         (x-td "Names of cities, towns, villages and smaller places."
                                               (wiki-list (list "City"
                                                                "Town"
                                                                "Village"
                                                                "Hamlet"
                                                                "Suburb"
                                                                "Quarter"
                                                                "Neighbourhood")))
                                         (x-td "Point"))))))))


;;;; Place files (the files that are indexed).

(defun latlon-normalized (lat lon &optional (precision 6))
  "#'round LAT LON to PRECISION decimal places and shift the values into the domain [0,180*10^PRECISION] for lat and [0,360*10^PRECISION] for lon.
   Accepts rendered string representations for LAT LON."
  (let ((lat-number (if (stringp lat) (read-from-string (format nil "~Ad0" lat)) lat))
        (lon-number (if (stringp lon) (read-from-string (format nil "~Ad0" lon)) lon)))
    ;;Not #'truncate, #'truncate wouldn't level out the values across all search index entries.
    (list :lat (round (* (+ lat-number 90) (expt 10 precision)))
          :lon (round (* (+ lon-number 180) (expt 10 precision))))))
(latlon-normalized -89.12345678d0 -179.12345678d0)
(latlon-normalized 89.12345678d0 179.12345678d0)
(latlon-normalized "89.12345678" "-179.123")

(defun latlon-from-normalized (normalized-lat normalized-lon &optional (precision 6))
  "Return the original (transportable) latitude and longitude values (as rationals)."
  (let ((domain-multiplier (expt 10 precision)))
    (flet ((denormalize (value shift)
             (/ (let ((a (- value (* shift domain-multiplier))))
                  ;;Round-trippable values.
                  (if (minusp a) (1+ a) a))
                domain-multiplier)))
      (list :lat (denormalize normalized-lat 90)
            :lon (denormalize normalized-lon 180)))))
;;..57 is correct (#'round in #'latlon-normalized).
(flet ((round-trip-test (lat lon)
         (let* ((n (latlon-normalized lat lon))
                (r (latlon-from-normalized (getf n :lat) (getf n :lon))))
           (format nil "~A ~,10F / ~A ~,10F"
                   (getf r :lat) (coerce (getf r :lat) 'double-float)
                   (getf r :lon) (coerce (getf r :lon) 'double-float)))))
  (list (round-trip-test -89.12345678d0 -179.12345678d0)
        (round-trip-test  -0.12345678d0   -0.12345678d0)
        (round-trip-test  -0.00000000d0   -0.00000000d0)
        (round-trip-test   0.00000000d0    0.00000000d0)
        (round-trip-test   0.12345678d0    0.12345678d0)
        (round-trip-test  89.12345678d0  179.12345678d0)))

;;Authority: TOO.
(defun snip-number-encode (&rest non-negative-integers)
  "Separated Numbers In Packet (SNIP) TOO edition: leveraging radix-alien barriers for a packed representation of NON-NEGATIVE-INTEGERS."
  ;;6: experiments showed dense packing. TODO: Benedikt Steger invented SNIP 2009 for other requirements. Radix choice really must get revalidated.
  (when non-negative-integers
    (parse-integer (format nil "~{~5R~^5~}" non-negative-integers) :radix 6)))
(format nil "~6R" (snip-number-encode 12 34 56 78))

(defun snip-overhead-in-bits (&rest non-negative-integers)
  "Helper that measures the number of bits invested for a self-describing SNIP structure with NON-NEGATIVE-INTEGERS in the order defined by NON-NEGATIVE-INTEGERS. Smaller is better."
  (- (integer-length (apply #'snip-number-encode non-negative-integers))
     (reduce #'+ non-negative-integers :key #'integer-length)))

;;Authority: TOO.
(defun snip-number-decode (snip-number)
  "Decode packed numbers in SNIP-NUMBER according to the SNIP method TOO edition."
  (labels ((split5 (&key string (start 0) accumulator)
             "Split STRING by #\5. START ACCUMULATOR are internal."
             (let ((barrier-pos (position #\5 string :start start)))
               (if barrier-pos
                   (split5 :string string
                           :start (1+ barrier-pos)
                           :accumulator (cons (subseq string start barrier-pos) accumulator))
                   (reverse (cons (subseq string start) accumulator))))))
    (mapcar (lambda (x) (if (zerop (length x))
                            0
                            (parse-integer x :radix 5)))
            (split5 :string (format nil "~6R" snip-number)))))
(snip-number-decode (snip-number-encode 12 34 56 78))
(snip-number-decode (snip-number-encode 12 0))
(snip-number-decode (snip-number-encode 0 34))
(snip-number-decode (snip-number-encode 0 0))
(snip-number-decode (snip-number-encode 12345678901234567890123456789 98765432109876543210987654321))

;;According to RFC 1738, #\$ #\- #\_ #\. #\+ #\! #\* #\' #\( #\) #\, are allowed without escaping (percent-encoding).
;;Testing fails with hAe-3+A+4UmEv't1)ee → removing #\'.
;;THINK: #\/ with #\?? → Works.
;;Removing #\/ since the locator payload is written in place name filenames now.
;;Removing #\\ because of #'make-pathname errors.
;;Removing #\? because of kiwix-serve.
;;0-9 are intentionally at the beginning because of #'defun-alphabetical-number-numerically.
(defparameter *payload-alphabet*
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!$()*+,-._")

(defun number-alphabetically (&key number alphabet accumulator)
  "Look up glyphs from an ALPHABET until NUMBER is consumed.
   ACCUMULATOR is internal."
  (let ((size (length alphabet)))
    (if (plusp number)
        (number-alphabetically
         :number (truncate number size)
         :alphabet alphabet
         :accumulator (cons (aref alphabet (rem number size)) accumulator))
        (format nil "~{~A~}" (or accumulator (list (aref alphabet 0)))))))
;;1234 (11 bits, #x4D2, #o2322, #b10011010010)
(number-alphabetically :number 1234 :alphabet "01")
(number-alphabetically :number 1234 :alphabet "01234567")
(number-alphabetically :number 1234 :alphabet "0123456789")
(number-alphabetically :number 1234 :alphabet "0123456789abcdef")
(number-alphabetically :number 1234 :alphabet *payload-alphabet*)
;;22511452115303
(number-alphabetically :number (snip-number-encode 12 34 56 78) :alphabet "012345")
(number-alphabetically :number 0 :alphabet "01")

(defun alphabetical-number-numerically (&key string alphabet (position 0) (accumulator 0))
  "Convert result of #'number-alphabetically back to a number.
   POSITION ACCUMULATOR are internal."
  (if (= position (length string))
      accumulator
      (alphabetical-number-numerically
       :string string :alphabet alphabet
       :position (1+ position)
       :accumulator (+ accumulator
                       (* (position (elt string position) alphabet)
                          (expt (length alphabet)
                                (- (length string) position 1)))))))
(alphabetical-number-numerically :string "10011010010" :alphabet "01")
(alphabetical-number-numerically :string "2322" :alphabet "01234567")
(alphabetical-number-numerically :string "1234" :alphabet "0123456789")
(alphabetical-number-numerically :string "4d2" :alphabet "0123456789abcdef")
(alphabetical-number-numerically :string "HA" :alphabet *payload-alphabet*)
(snip-number-decode (alphabetical-number-numerically :string "22511452115303" :alphabet "012345"))

(defun locator-payload-encode (filename-rank ll-lat ll-lon &optional ur-lat ur-lon)
  "Encode a payload that will get written into place file filenames according to following specification:
   The geometry is either a point (given by LL-LAT LL-LON) or a MBR (given by LL-LAT LL-LON UR-LAT UR-LON).
   The geometry arguments are rationals or possibly rendered (double-)floats, ranging from -90 to 90 for the lat value, and from -180 to 180 for the lon value. Exactly 6 decimal places are considered (#'round).
   The intermediate representation normalizes the values (#'latlon-normalized), prepends FILENAME-RANK (if non-nil) and packs them (#'snip-number-encode) in a number, the ur being delta encoded if a MBR is considered.
   Finally, the number is represented in a target alphabet defined by *payload-alphabet*.
   
   FILENAME-RANK can be NIL and differs from given numbers (including 0) in such a case."
  (let* ((ll-norm (latlon-normalized ll-lat ll-lon))
         (ur-norm (when (and ur-lat ur-lon)
                    (latlon-normalized ur-lat ur-lon)))
         (geometric-payload (if ur-norm
                                (list (getf ll-norm :lat) (getf ll-norm :lon)
                                      (- (getf ur-norm :lat) (getf ll-norm :lat))
                                      (- (getf ur-norm :lon) (getf ll-norm :lon)))
                                (list (getf ll-norm :lat) (getf ll-norm :lon))))
         (final-payload (if filename-rank (cons filename-rank geometric-payload) geometric-payload)))
    (number-alphabetically :number (apply #'snip-number-encode final-payload)
                           :alphabet *payload-alphabet*)))
(locator-payload-encode 0 -89.12345678d0 -179.12345678d0 89.12345678d0 179.12345678d0)
(locator-payload-encode nil -89.12345678d0 -179.12345678d0)

(defun locator-payload-decode (number)
  "Decode the geometry portion of a locator payload. For the specification, see #'locator-payload-encode."
  (let* ((as-number (alphabetical-number-numerically :string number :alphabet *payload-alphabet*))
         ;;Even/odd: unranked/ranked, length 4 or higher: MBR, not point.
         (snipped (snip-number-decode as-number))
         (rank-offset (rem (length snipped) 2))
         (ll-lat (elt snipped rank-offset))
         (ll-lon (elt snipped (1+ rank-offset)))
         (ll (latlon-from-normalized ll-lat ll-lon)))
    (if (= (+ rank-offset 4) (length snipped))
        (let* ((ur-lat (+ (elt snipped (+ 2 rank-offset)) ll-lat))
               (ur-lon (+ (elt snipped (+ 3 rank-offset)) ll-lon))
               (ur (latlon-from-normalized ur-lat ur-lon)))
          (list :ll-lat (getf ll :lat) :ll-lon (getf ll :lon)
                :ur-lat (getf ur :lat) :ur-lon (getf ur :lon)))
        (list :ll-lat (getf ll :lat) :ll-lon (getf ll :lon)))))
(let ((mbr (locator-payload-decode
            (locator-payload-encode 2 -89.12345678d0 -179.12345678d0 89.12345678d0 179.12345678d0))))
  (format nil "~,16F ~,16F / ~,16F ~,16F"
          (coerce (getf mbr :ll-lat) 'double-float)
          (coerce (getf mbr :ll-lon) 'double-float)
          (coerce (getf mbr :ur-lat) 'double-float)
          (coerce (getf mbr :ur-lon) 'double-float)))
#|(let ((mbr (locator-payload-decode "")))
  (format nil "~,10F ~,10F"
          (coerce (getf mbr :ll-lat) 'double-float)
          (coerce (getf mbr :ll-lon) 'double-float)))|#


;;; Required client-side JavaScript versions of some previously introduced functions.

(defun defun-alphabetical-number-numerically (&optional (alphabet *payload-alphabet*))
  "#'alphabetical-number-numerically as client-side JavaScript function definition which allows for locator payload decoding in the (locator) map.
   Is called \"convertBase\" in JavaScript."
  (dsl #\Newline
       "/*The function convertBase is adapted from StackOverflow answer 55011290 (Slavik Meltser, CC-BY-SA 4.0), which itself is based on https://www.danvk.org/hex2dec.html.*/"
       (dsl '("function convertBase(str, fromBase, toBase) {" ";" "}")
            (format nil "const DIGITS = \"~A\"" (escape-js alphabet))
            (dsl '("const add = (x, y, base) => {" ";" "}")
                 "let z = []"
                 "const n = Math.max(x.length, y.length)"
                 "let carry = 0"
                 "let i = 0"
                 (dsl '("while (i < n || carry) {" ";" "}")
                      "const xi = i < x.length ? x[i] : 0"
                      "const yi = i < y.length ? y[i] : 0"
                      "const zi = carry + xi + yi"
                      "z.push(zi % base)"
                      "carry = Math.floor(zi / base)"
                      "i++")
                 "return z")
            (dsl '("const multiplyByNumber = (num, x, base) => {" ";" "}")
                 "if (num < 0) return null"
                 "if (num == 0) return []"
                 "let result = []"
                 "let power = x"
                 (dsl '("while (true) {" ";" "}")
                      "num & 1 && (result = add(result, power, base))"
                      "num = num >> 1"
                      "if (num === 0) break"
                      "power = add(power, power, base)")
                 "return result")
            (dsl '("const parseToDigitsArray = (str, base) => {" ";" "}")
                 "const digits = str.split('')"
                 "let arr = []"
                 (dsl '("for (let i = digits.length - 1; i >= 0; i--) {" ";" "}")
                      "const n = DIGITS.indexOf(digits[i])"
                      "if (n == -1) return null"
                      "arr.push(n)")
                 "return arr")
            "const digits = parseToDigitsArray(str, fromBase)"
            "if (digits === null) return null"
            "let outArray = []"
            "let power = [1]"
            (dsl '("for (let i = 0; i < digits.length; i++) {" ";" "}")
                 "digits[i] && (outArray = add(outArray, multiplyByNumber(digits[i], power, toBase), toBase))"
                 "power = multiplyByNumber(fromBase, power, toBase)")
            "let out = ''"
            (dsl '("for (let i = outArray.length - 1; i >= 0; i--) {" ";" "}")
                 "out += DIGITS[outArray[i]]")
            "return out")))

(defun defun-latlon-from-normalized ()
  "#'latlon-from-normalized as client-side Javascript function definition with a hardcoded precision of 6."
  ;;Authority: #'latlon-from-normalized.
  (dsl '("function latlonFromNormalized(normalizedLat,normalizedLon) {" ";" "}")
       "const domainMultiplier=Math.pow(10,6)"
       (dsl '("const denormalize = (value,shift) => {" ";" "}")
            "let a=value-(shift*domainMultiplier)"
            "return (((a<0) ? (a+1) : (a))/domainMultiplier)")
       "return new Object({lat:denormalize(normalizedLat,90),lon:denormalize(normalizedLon,180)})"))

(defun defun-locator-payload-decode ()
  "#'locator-payload-decode as client-side Javascript function definition."
  ;;Authority: #'locator-payload-decode.
  (dsl '("function locatorPayloadDecode(number) {" ";" "}")
       (dsl '("let snipped=" "." "")
            (format nil "convertBase(number,~A,6)" (length *payload-alphabet*))
            "split('5')"
            (dsl '("map(function (x){" ";" "})")
                 "let result=convertBase(x,5,10)"
                 (dsl " " "return" (jsq-ternary "result===''" "0" "parseInt(result)"))))
       "let rankOffset=snipped.length%2"
       "let llLat=snipped[rankOffset]"
       "let llLon=snipped[rankOffset+1]"
       "let ll=latlonFromNormalized(llLat,llLon)"
       ;;4: number of values needed to defined a MBR.
       (dsl '("if(rankOffset+4==snipped.length){" "}else{" "}")
            (dsl ";"
                 "let urLat=snipped[rankOffset+2]+llLat"
                 "let urLon=snipped[rankOffset+3]+llLon"
                 "let ur=latlonFromNormalized(urLat,urLon)"
                 "return new Object({llLat:ll.lat,llLon:ll.lon,urLat:ur.lat,urLon:ur.lon})")
            "return new Object({llLat:ll.lat,llLon:ll.lon})")))
(snip-number-decode (alphabetical-number-numerically :string "2cuqd)6.SJ!K_6" :alphabet "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ$-_.+!*(),/"))

(defun place-file-builder ()
  "Client-side Javascript file which builds a usable document out of the minimalistic place files.
   Creates an iframe with the locator map file.
   Depends on locator-map.html in the same directory (handled by user-interface.lisp)."
  (dir-entry
   :url "_"
   :mimetype-id :js
   :content (dsl ";"
                 ;;Just in case this "article" is opened somehow.
                 "/*See TOO.zip/README.md if you're interested in the inner workings of this ZIM file. TOO.zip is linked in the \"Technical information\" section of the welcome file.*/"
                 
                 "document.body=document.createElement('body')"
                 "document.body.setAttribute('style','margin:0;padding:0;overflow:hidden')"
                 
                 ;;kiwix-serve's suggestion box is partially hidden otherwise.
                 "mystyle=document.createElement('style')"
                 "mystyle.innerText='.ui-menu{z-index:101}'"
                 "document.head.appendChild(mystyle)"
                 
                 "myframe=document.createElement('iframe')"
                 "myframe.setAttribute('style','border-width:0;width:100%;height:100%;padding:0em;overflow:hidden')"
                 ;;"var scripts = document.getElementsByTagName('script')"
                 ;;"myframe.setAttribute('src','locator-map.html?l'+scripts[scripts.length-1].src.split('?')[1])"
                 "var locatorNumber=window.location.pathname.toString().split('/')"
                 "locatorNumber=locatorNumber[locatorNumber.length-1]"
                 ;;4: (length ".htm")
                 ;;"locatorNumber=locatorNumber.substr(0,locatorNumber.length-4)"
                 "myframe.setAttribute('src','locator-map.html?location='+locatorNumber+'&title='+document.title)"
                 "document.body.appendChild(myframe)")))

(defun place-file (&key filename-rank title ll-lat ll-lon ur-lat ur-lon)
  "The minimalistic index entry for name lookups.
   Depends on the file returned by #'place-file-builder.
   Returns a `dir-entry instance."
  (dir-entry
   :url (locator-payload-encode filename-rank ll-lat ll-lon ur-lat ur-lon)
   :title title
   :mimetype-id :html
   :content (format nil
                    ;;TODO: test whether all browsers accept an unquoted attribute value and missing end tags.
                    ;;Reason for no <html>: because it works.
                    ;;Reason for <head> <body>: kiwix-serve isn't able to inject its interface completely otherwise. While it is possible to unblock the interface in the map, the links would be caught in the iframe, which results in a fixed title (the place name) and outright failures when choosing the "random article" link.
                    ;;Reason for utf8: it's impossible to inject it dynamically later.
                    ;;Reason for <title>: bookmarks and a clear interface for the users.
                    ;;Reason for path / name: shortest form possible (also see #'place-file-builder).
                    ;;Reason for </script>: tested browsers don't accept an unclosed <script>.
                    "<head><meta charset=utf8><title>~A</title><script src=_></script><body>"
                    ;;THINK: is HTML escaping needed? ZIM index seems correct. Potentially a problem security-wise, though... → Deactivating #\< to be really sure.
                    (substitute #\_ #\< title))))
(title (place-file :filename-rank 0 :title "ABC</title><script>DEF</script><title>GHI" :ll-lat "1.234567" :ll-lon "7.654321"))
#|(loop for i from 0 below 2000000
      collect (place-file :title "ABC" :ll-lat "1.23" :ll-lon "4.56"))|#


;;;; DB → place-file conversion-related

#|
TESTING: The following disambiguation code is zimchecked and produces no duplicates.

The three collision domains are: multipart geometries (AREA_RANK), name+placetag (NAMEPLACE_RANK), effective geometry (filename rank).
Filename rank is determined on the Lisp side because of the impedance mismatch between #'latlon-normalized and ST_SnapToGrid(way/mbr,0.000001) or ST_AsText(way/mbr,6), respectively.
Required columns of the building SQL query: ENTRY_TYPE POTENTIALLY_TRANSLATED_NAME LATLON LL_LON LL_LAT UR_LAT UR_LON AREA_COUNT AREA_RANK NAMEPLACE_COUNT NAMEPLACE_RANK.
AREA_COUNT AREA_RANK default to 1 if the tuples are a point.
(and (= 1 NAMEPLACE_COUNT) (= 1 NAMEPLACE_RANK)) means: no collision (only 1 entry counted).
Either LATLON for points (as returned by #'as-latlon) or LL_LON LL_LAT UR_LON UR_LAT for MBRs. The latter are shadowed if the former is not :NULL.
Note that MBRs can be calculated for both polygon and line geometries (even for points, but that would be confusing for users).
|#

(defun filtered-and-translated (table-name &optional (where "TRUE"))
  "SQL table with UNIONed NAME and TAGS->'name:en' column as POTENTIALLY_TRANSLATED_NAME (non-NULL)."
  (dsl '(" AS ")
       (dsl '("((" ")UNION(" "))")
            (loop for source in '("name" "tags->'name:en'")
                  collect (dsl " "
                               "SELECT" (dsl "," "*" (dsl " " source "potentially_translated_name"))
                               "FROM" table-name
                               "WHERE" (dsl '("(" ") AND (" ")")
                                            (dsl " " source "IS NOT NULL")
                                            where))))
       "filtered_and_translated"))
(dsl '("SELECT potentially_translated_name FROM" "" ";")
     (filtered-and-translated "planet_osm_point" "tags @> 'capital=>yes'"))
#|(dsl '("SELECT potentially_translated_name FROM" "" ";")
     (filtered-and-translated "planet_osm_polygon" (mbr-filter "TRUE" (geometry *selected-flavour*))))|#

(defun nameplace-collision-columns (&optional (entry-type "place"))
  "SQL \"collision\" columns NAMEPLACE_COUNT NAMEPLACE_RANK.
   The columns access POTENTIALLY_TRANSLATED_NAME PLACE (through ENTRY-TYPE) TAGS->'population' OSM_ID."
  (let ((partition-by (format nil "PARTITION BY CONCAT(potentially_translated_name,COALESCE(~A,''))" entry-type)))
    (list (dsl '(" AS ")
               (format nil "COUNT(*) OVER (~A)" partition-by)
               "nameplace_count")
          (dsl '(" AS ")
               (dsl '("row_number() OVER (" " " ")")
                    partition-by
                    (dsl '("ORDER BY " "," "")
                         (dsl " "
                              "CASE"
                              "WHEN (tags->'population' ~ '^[0-9]{1,8}$') THEN (tags->'population')::INTEGER"
                              "WHEN (place = 'city') THEN 100000"
                              "WHEN (place = 'town') THEN 1000"
                              "ELSE 1"
                              "END"
                              "DESC")
                         "array_length(akeys(tags),1) DESC"
                         "abs(osm_id) ASC"))
               "nameplace_rank"))))

(defun point-place-requirements (entry-type)
  "List of columns that are expected by name queries.
   For tuples that represent a point.
   ENTRY-TYPE: (escaped) literal SQL string."
  (asplice (dsl '(" AS ") entry-type "entry_type")
           "potentially_translated_name"
           (as-latlon "way") "NULL ll_lon" "NULL ll_lat" "NULL ur_lat" "NULL ur_lon"
           "1 area_count" "1 area_rank"
           (nameplace-collision-columns entry-type)))

(defun ranked-mbrs-requirements (entry-type)
  "List of columns that are expected by #'ranked-mbrs.
   ENTRY-TYPE: (escaped) literal SQL string."
  (asplice (dsl '(" AS ") entry-type "entry_type")
           "osm_id"
           "potentially_translated_name"
           "(ST_Dump(way)).geom single_geom"
           "ST_NumGeometries(way) area_count"
           (nameplace-collision-columns entry-type)))
(ranked-mbrs-requirements "'country'")

(defun ranked-mbrs (single-polygons where)
  "A MBR-returning query that satisfies the requirements of name queries.
   Introduces the AREA_RANK column to the query in SINGLE-POLYGONS and determines the MBR corners.
   SINGLE-POLYGONS has to provide the columns OSM_ID ENTRY_TYPE POTENTIALLY_TRANSLATED_NAME SINGLE_GEOM AREA_COUNT NAMEPLACE_COUNT NAMEPLACE_RANK.
    A helper called #'ranked-mbrs-requirements exists.
   You can see this function as a SQL view."
  (sqlq-let* (sqlq-let* single-polygons
                        (list "ST_Transform(ST_SetSRID(Box2D(single_geom),3857),4326) mbr"))
             (list "ST_YMin(mbr) ll_lat" "ST_XMin(mbr) ll_lon" "ST_YMax(mbr) ur_lat" "ST_XMax(mbr) ur_lon"
                   "NULL latlon"
                   "dense_rank() OVER (PARTITION BY osm_id ORDER BY ST_Area(single_geom) DESC) AS area_rank")
             (list "entry_type" "potentially_translated_name" "area_count" "nameplace_count" "nameplace_rank")
             where))

(defun name-queries (flavour)
  "SQL queries that deliver index entries of a FLAVOUR.
   Adapted from openstreetmap-carto's project.mml.
   Merging the queries into one per geometry type results in easier code reuse and is robust since it is guaranteed that no unnecessary duplicates are fetched.
    The queries are logically equivalent to the user-communicated ones.
    The same geometry may occur multiple times because of different admin levels."
  (let ((place-qualifier (dsl '(" IN ")
                              "place"
                              (dsl "(,)"
                                   "'city'"
                                   "'town'"
                                   "'village'"
                                   "'hamlet'"
                                   "'suburb'"
                                   "'quarter'"
                                   "'neighbourhood'"))))
    (list
     ;;Polygon: country names and admin names.
     (ranked-mbrs
      (dsl " "
           "SELECT" (dsl ","
                         (ranked-mbrs-requirements "CASE WHEN admin_level='2' THEN 'country' WHEN admin_level IS NOT NULL THEN concat('admin',admin_level) ELSE place END"))
           "FROM"
           (filtered-and-translated
            "planet_osm_polygon"
            (mbr-filter
             (dsl '("(" " OR " ")")
                  ;;TODO: resolve name collisions between points and polygons ("London (city)").
                  ;;place-qualifier
                  (dsl '("(" " AND " ")")
                       "boundary = 'administrative'"
                       (dsl '("(" " OR " ")")
                            (loop for level from 2 upto 8
                                  collect (format nil "admin_level = '~A'" level)))
                       ;;osm2pgsql uses negative OSM IDs for relations.
                       "osm_id < 0"))
             (geometry flavour))))
      ;;Apply the ZIM file region to all singlepart geometries.
      (mbr-filter "TRUE" (geometry flavour) (sqlq-call-function "ST_Transform" "mbr" "3857")))
     ;;Point: capital names and placenames.
     (dsl " "
          "SELECT"
          (dsl "," (point-place-requirements "CASE WHEN tags@>'capital=>yes' THEN concat('capital',case when place is null then '' else concat(' ',place) end) ELSE place END"))
          "FROM"
          (filtered-and-translated
           "planet_osm_point"
           (mbr-filter
            (dsl '("(" " OR " ")")
                 ;;Not filtering the place tag because of Funafuti (place=municipality) and Beijing (no place tag).
                 "tags @> 'capital=>yes'"
                 (dsl '("(" " AND " ")")
                      "(NOT (tags @> 'capital=>yes'))"
                      place-qualifier))
            (geometry flavour)))))))
;;(format T "~A" (name-queries *selected-flavour*))

(defun loc-files (flavour)
  "A list of all indexed places (as dir-entries) plus the #'place-file-builder. Returns a list of `dir-entry instances.
   D1 D2 etc. are appended if (< 1 NAMEPLACE_COUNT).
   A1 A2 etc. are appended if (< 1 AREA_COUNT). A1 A2 etc are appended regardless whether D1 is already appended or not."
  (let ((index-entries nil)
        ;;THINK: #'sxhash equality works with lists with four entries. 'eql hash table? → No, #'sxhash has no standard-mandated guarantees for such a (SBCL-only?) behaviour.
        (filename-disambiguation (make-hash-table :test 'equal)))
    (loop for name-query in (name-queries flavour)
          do (querym name-query
               (loop for row in rows
                     do (let* ((entry-type (my-assoc "entry_type" row))
                               (potentially-translated-name (my-assoc "potentially_translated_name" row))
                               (latlon (my-assoc "latlon" row))
                               (ll-lat (my-assoc "ll_lat" row))
                               (ll-lon (my-assoc "ll_lon" row))
                               (ur-lat (my-assoc "ur_lat" row))
                               (ur-lon (my-assoc "ur_lon" row))
                               (filename-rank (incf (gethash (list ll-lat ll-lon ur-lat ur-lon) filename-disambiguation 0)))
                               (area-count (my-assoc "area_count" row))
                               (area-rank (my-assoc "area_rank" row))
                               (nameplace-count (my-assoc "nameplace_count" row))
                               (nameplace-rank (my-assoc "nameplace_rank" row)))
                          (unless (eql :NULL potentially-translated-name)
                            (push (place-file
                                   ;;Assuming that bookmarks are local to the ZIM-file.
                                   :filename-rank (when (< 1 filename-rank)
                                                    ;;2: the first case (1; the majority of entries) is disambiguated by passing a nil in the function call.
                                                    (- filename-rank 2))
                                   ;;Appending the disambiguations since it is easier to detect - for example - all single geometries through the search bar of kiwix-desktop this way.
                                   :title (format nil "~A (~A)~@[ D~A~]~@[ A~A~]"
                                                  potentially-translated-name
                                                  entry-type
                                                  ;;Nameplace disambiguation.
                                                  (when (< 1 nameplace-count)
                                                    nameplace-rank)
                                                  ;;Area disambiguation.
                                                  (when (< 1 area-count)
                                                    area-rank))
                                   :ll-lat (if (eql :NULL latlon)
                                               ll-lat
                                               (subseq latlon 0 (position #\Space latlon)))
                                   :ll-lon (if (eql :NULL latlon)
                                               ll-lon
                                               (subseq latlon (1+ (position #\Space latlon))))
                                   :ur-lat (unless (eql :NULL ur-lat) ur-lat)
                                   :ur-lon (unless (eql :NULL ur-lon) ur-lon))
                                  index-entries))))))
    (cons
     (place-file-builder)
     index-entries)))
;;(loc-files *selected-flavour*)

