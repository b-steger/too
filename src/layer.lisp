;;;; TOO (The Offline Oriented) creates ZIM files with offline available maps
;;;; Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>
;;;; 
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :too)

;;;; Prepare layers for The Offline Oriented.

(defparameter *layers*
  (make-hash-table :test #'equal)
  "name (string) → `layer instance")

(defparameter *layer-entries-count*
  (make-hash-table :test #'equal)
  "name (string) → integer denoting the number of returned rows.")

(defclass layer-column ()
  ((expression :accessor expression :initarg :expression :initform nil :documentation "SQL")
   (alias :accessor alias :initarg :alias :initform nil :documentation "SQL")
   (user-facing-name :accessor user-facing-name :initarg :user-facing-name :initform nil))
  (:documentation "A column of a layer, included in the tooltip."))

;;TODO: depending on :point :line :polygon, dynamically select the column or the tag expression.
(defun quick-tag (alias &optional own-expression user-facing-name)
  "Create a layer-column instance.
   Use OWN-EXPRESSION for
    tags that have their own column.
    columns with an own expression."
  (make-instance 'layer-column
                 :expression (or own-expression
                                 (format nil "tags->'~A'" alias))
                 :alias alias
                 :user-facing-name (or user-facing-name alias)))

(defclass layer ()
  ((name :accessor name :initarg :name :initform nil :documentation "Used in file names, Javascript and SQL.")
   (category :accessor category :initarg :category :initform nil)
   (user-facing-name :accessor user-facing-name :initarg :user-facing-name :initform nil)
   (map-symbol :accessor map-symbol :initarg :map-symbol :initform nil :documentation "Map symbol SVG content.")
   (own-description-paragraphs :accessor own-description-paragraphs :initarg :own-description-paragraphs :initform nil)
   (wiki-definitions :accessor wiki-definitions :initarg :wiki-definitions :initform nil :documentation "List of wiki-definition keys.")
   (columns :accessor columns :initarg :columns :initform nil :documentation "List of layer-column instances.")
   (predicate :accessor predicate :initarg :predicate :initform nil :documentation "SQL")
   (user-facing-predicate :accessor user-facing-predicate :initarg :user-facing-predicate :initform nil)
   (renderer :accessor renderer :initarg :renderer :initform nil :documentation "Lambda accepting STREAM and ROW.")
   (popup-name-renderer :accessor popup-name-renderer :initarg :popup-name-renderer :initform nil :documentation "Lambda accepting NAME (of layer). Should return JavaScript code, context see #'map-file-leaflet. If NIL (default), the name column is passed through."))
  (:documentation "All needed informations to fetch, render and describe a layer."))

(defun register-layer (&key name category user-facing-name map-symbol own-description-paragraphs wiki-definitions columns predicate user-facing-predicate renderer popup-name-renderer)
  "Quick helper function which registers an entry in *layers*.
   Side-effecting."
  (setf (gethash name *layers*)
        (make-instance
         'layer
         :name name
         :category category
         :user-facing-name user-facing-name
         :map-symbol map-symbol
         :own-description-paragraphs own-description-paragraphs
         :wiki-definitions wiki-definitions
         :columns columns
         :predicate predicate
         :user-facing-predicate user-facing-predicate
         :renderer renderer
         :popup-name-renderer popup-name-renderer)))

(defgeneric map-symbol-color (color-category)
  (:documentation
   "The color in HTML/SVG notation (e.g. #00aa00) or \"none\".")
  (:method ((color-category (eql :amenity)))
    "library social_facility shower toilets drinking_water police"
    "#4d2efe")
  (:method ((color-category (eql :transport)))
    "fuel bus_stop"
    "#2a4b4a")
  (:method ((color-category (eql :health)))
    "hospital doctors pharmacy"
    "#009000")
  (:method ((color-category (eql :shelter)))
    "shelter"
    "#000000")
  (:method ((color-category (eql :shop)))
    "shop=*"
    "#b62c14")
  (:method ((color-category T))
    "Undefined."
    "none"))
(map-symbol-color :shop)

(defun map-symbol-svg (color-category &rest paths)
  "The SVG content of a map symbol.
   Valid values for COLOR-CATEGORY are derived from the specializers of #'map-symbol-color."
  (x-svg :xmlns "http://www.w3.org/2000/svg"
         :width "15"
         :height "15"
         :viewBox "0 0 15 15"
         (loop for path in paths
               collect (x-path :d path
                               :fill (map-symbol-color color-category)))))
(map-symbol-svg :none "abc 123" "def 456")

;;TODO: cursors (DECLARE/FETCH)
(defmacro querym (query &body body)
  "Query the database and provide the variable `rows."
  `(let ((rows (cl-postgres:exec-query *connection* ,query 'cl-postgres:alist-row-reader)))
     ,@body))

(defun my-assoc (column row)
  "Return the value of a column/parameter name of a row/parameter (which is a part of an alist)."
  (cdr (assoc column row :test #'string-equal)))

(defun snapped-centroid (&optional (source "way"))
  "Fast and good enough way to reduce lines and polygons to a point.
   The resulting point is the centroid of SOURCE if it is within SOURCE, the closest point on SOURCE to the centroid otherwise.
   Returns a rendered sql-column."
  ;;TODO: inscribed circle for everything was prohibitively slow, but perhaps a CASE calling ST_MaximumInscribedCircle if the centroid is not within the polygon is fast enough, too.
  (sqlq-call-function "ST_ClosestPoint"
                      source
                      (sqlq-call-function "ST_Centroid"
                                          source)))
(snapped-centroid)
(snapped-centroid "geom")

(defun as-latlon (point &optional (alias "latlon"))
  "Convert POINT (a SQL expression) to latlontext representation on the SQL side."
  (flet ((sc (&rest arguments)
           (apply #'sqlq-call-function arguments)))
    (dsl '(" AS ")
         (sc "ST_AsLatLonText"
             (sc "ST_Transform"
                 point
                 "4326")
             (dsl :squote "DDD.DDDDDDDDD"))
         alias)))
(as-latlon "way")

(defun present-for-schoolclass (&rest functions-and-argument-at-last)
  "All elements in (butlast FUNCTIONS-AND-ARGUMENT-AT-LAST) call the rest until the present, i.e. (last FUNCTIONS-AND-ARGUMENT-AT-LAST), is encountered.
   (Actually only renders with #'sqlq-call-function.)"
  (if (cdr functions-and-argument-at-last)
      (sqlq-call-function
       (car functions-and-argument-at-last)
       (apply #'present-for-schoolclass (cdr functions-and-argument-at-last)))
      (car functions-and-argument-at-last)))
(present-for-schoolclass "abc" "def" "ghi" "jkl" "mno" "present-at-last")

(defun mbr-filter (original-predicate mbr &optional (geom-column "way"))
  "Enhance ORIGINAL-PREDICATE with MBR intersecting filter if the MBR is not #'clamp-mbr -global.
   MBR: property list with LL-LAT LL-LON UR-LAT UR-LON."
  (if (equal-mbr? mbr 
                  (clamp-mbr (make-instance 'mbr-4326 :ll-lat -90 :ll-lon -180
                                                      :ur-lat 90 :ur-lon 180)))
      original-predicate
      (dsl '("((" ") AND (" "))")
           #|;;Adapted from openstreetmap-carto.
           (dsl '(" && ")
           (sqlq-call-function "ST_PointOnSurface" geom-column)
           )|#
           ;;Intersection includes bordering geometries, which is useful.
           (sqlq-call-function
            "ST_Intersects"
            geom-column
            (sqlq-call-function "ST_SetSRID"
                                (flet ((defpoint (lat lon)
                                         (sqlq-call-function
                                          "ST_Transform"
                                          (sqlq-call-function
                                           "ST_SetSRID"
                                           (sqlq-call-function "ST_MakePoint" lon lat)
                                           "4326")
                                          "3857")))
                                  (sqlq-call-function
                                   "ST_MakeBox2D"
                                   (defpoint (ll-lat mbr) (ll-lon mbr))
                                   (defpoint (ur-lat mbr) (ur-lon mbr))))
                                "3857"))
           original-predicate)))
(mbr-filter "1==2" (make-instance 'mbr-4326 :ll-lat -12.34 :ll-lon -56.78 :ur-lat 12.34 :ur-lon 56.78))

(defun layer-query (&key layer flavour)
  "Convert the layer instance to a SQL query. Expects NAME in (columns layer)."
  (dsl ""
       (dsl " "
            "SELECT"
            (dsl #\,
                 "latlon"
                 (mapcar #'alias (columns layer)))
            "FROM"
            (dsl " "
                 (dsl (list "(" (format nil " UNION ALL ") ")")
                      (loop for table in '("point" "line" "polygon")
                            collect (dsl "( )"
                                         "SELECT"
                                         (dsl
                                          ","
                                          ;;TODO: avoid projection round-trip.
                                          ;;Precision 9: default of PostGIS' ST_AsGeoJSON.
                                          (as-latlon (snapped-centroid))
                                          "way"
                                          (mapcar (lambda (x)
                                                    (if (equal (expression x) (alias x))
                                                        (alias x)
                                                        (dsl " " (expression x) (alias x))))
                                                  (columns layer)))
                                         (format nil "FROM planet_osm_~A" table)
                                         ;;vaccination_centre for non-emergency
                                         "WHERE"
                                         (mbr-filter (predicate layer) (geometry flavour)))))
                 (dsl :quote (name layer)))
            ;;IDEA: PruneCluster mentions O(n). Something with bounds. Precalculate the bounding box and feed it to the algorithm. Or precluster the data on the server side.
            ;;ORDER BY way alone does not make the expected difference. Prefer alphabetic sort until an alphabetic index solution is realized.
            "ORDER BY"
            (dsl ","
                 (dsl :quote "name")
                 (present-for-schoolclass
                  "ST_Centroid"
                  "ST_Transform"
                  "way,4326")))
       ";"))
;;(layer-query :layer (gethash "police_stations" *layers*) :flavour (nth 0 *flavours*))

(defun layer-rows (&key layer flavour)
  "Perform a SQL query and fetch all features, as defined by LAYER (an instance of `layer).
   Side-effecting: updates *layer-entries-count*."
  (let ((query (layer-query :layer layer :flavour flavour)))
    (querym query
      (setf (gethash (name layer) *layer-entries-count*) (length rows))
      (values rows query))))
;;(layer-rows :layer (gethash "hospitals" *layers*) :flavour (nth 0 *flavours*))

(defun layer-cache-file-location (layer flavour)
  "The location where the fetched layer file is stored."
  (format nil "/dev/shm/layer-~A-~A.js"
          ;;Keep results across development runs.
          ;;(gensym (name layer))
          (name layer)
          (nickname flavour)))

;;TODO: PBF file for space-efficiency.
(defun create-clusterable-layer (&key layer flavour)
  "Prepare a layer for PruneCluster. Returns the path to the file.
   LAYER: instance of `layer.
   Contract:
    the first line contains the number of feature entries, wrapped in a comment ('/*' and '*/').
    the JavaScript file populates a variable named (name layer), suitably structured for #'map-file-leaflet-js's destructuring code.
   Side-effecting: creates cached layer files and updates *layer-entries-count* (through the call to #'layer-rows)."
  (if (and (columns layer) (predicate layer))
      (let ((outfile (layer-cache-file-location layer flavour)))
        (unless (probe-file outfile)
          (with-open-file (stream outfile :direction :output :if-exists :supersede :if-does-not-exist :create)
            (let ((rows (layer-rows :layer layer :flavour flavour)))
              (format stream "/*~A*/~%" (length rows))
              (format stream "/*SQL query:~%~A~%*/~%" (layer-query :layer layer :flavour flavour))
              (format stream "var ~A=[~%" (name layer))
              (loop for (row . d) on rows
                    do (funcall (renderer layer) stream row)
                    when d
                      do (princ #\, stream)
                    do (princ #\Newline stream))
              (princ "]" stream))))
        outfile)
      ;;Assume a custom layer preparation.
      (format nil "/dev/shm/layer-~A.js" (name layer))))
;;(create-clusterable-layer :layer (gethash "hospitals" *layers*))

(defmacro tooltip-feature (&body body)
  "Basic feature entry renderer. Expects column LATLON."
  `(lambda (stream row)
     (let ((latlon (my-assoc "latlon" row)))
       (format stream "[~A,~A,"
               (subseq latlon 0 (position #\Space latlon)) (subseq latlon (1+ (position #\Space latlon))))
       ,@body
       (princ #\] stream))))

;;THINK: expose the columns to client's Javascript?
(defmacro tooltip-feature-addressable (&body body)
  "Renderer for features with an postal address, phone, and/or email address.
   Expects columns NAME PHONE EMAIL STREET HOUSENUMBER."
  `(tooltip-feature
     (let ((name (my-assoc "name" row))
           (phone (my-assoc "phone" row))
           (email (my-assoc "email" row))
           (street (my-assoc "street" row))
           (housenumber (my-assoc "housenumber" row)))
       (format stream "\"~@[~A~]~@[~A~]~@[<br>Phone: ~A~]~@[<br>Email: ~A~]"
               (if (eql :null name)
                   "???"
                   (escape-js name))
               (unless (and (eql :null street) (eql :null housenumber))
                 (format nil "<br>Address: ~A~@[ ~A~]"
                         (if (eql :null street)
                             "???"
                             (escape-js street))
                         (unless (eql :null housenumber)
                           (escape-js housenumber))))
               (unless (eql :null phone)
                 (escape-js phone))
               (unless (eql :null email)
                 (escape-js email)))
       ,@body
       (princ #\" stream))))

(defmacro tooltip-feature-addressable-opening-hours (&body body)
  "tooltip-feature-addressable, but with additional OPENINGHOURS."
  `(tooltip-feature-addressable
     (let ((opening-hours (my-assoc "openinghours" row)))
       (format stream "~@[<br>Open: ~A~]"
               (unless (eql :null opening-hours)
                 (escape-js opening-hours)))
       ,@body)))

(defun layer-name-0nf (layer flavour &optional with-symbol? (symbol-levels-up 0))
  "The final name for the layer which is shown to the user."
  (format nil "~@[~A~]~A: <b>~A</b> (~A)"
          (when with-symbol?
            (x-img :src (dsl ""
                             (loop for i from 0 below symbol-levels-up
                                   collect "../")
                             "../I/symbols/"
                             (name layer)
                             ".svg")
                   :style "height:1em;margin-right:0.5em"))
          (category layer) (user-facing-name layer) (or (gethash (name layer) *layer-entries-count*)
                                                        ;;*layer-entries-count* from previous Lisp images is obviously lost, fetch and remember the number of entries in LAYER.
                                                        (let ((layer-file (layer-cache-file-location layer flavour)))
                                                          (when (probe-file layer-file)
                                                            (with-open-file (f layer-file)
                                                              (let ((first-line (read-line f)))
                                                                (setf (gethash (name layer) *layer-entries-count*)
                                                                      (parse-integer (subseq first-line 2) :junk-allowed T))))))
                                                        "???")))
;;(layer-name-0nf (gethash "hospitals" *layers*) (elt *flavours* 6) T 1)


;;;; Layer data

;;TODO: entrance to huge areas, not the #'snapped-centroid.
;;      Pro: fits to the point representation used by TOO
;;      Contra: VGI quality discussion.
;;      → Better focus on the lowest common denominator. → Use entrance if available, #'snapped-centroid otherwise → attribute merging? → KISS: worst case are two features for the same hospital.
(register-layer
 :name "hospitals"
 :category "Health"
 :user-facing-name "Hospitals"
 :map-symbol (map-symbol-svg :health "M7,1C6.4,1,6,1.4,6,2v4H2C1.4,6,1,6.4,1,7v1&#xA;&#x9;c0,0.6,0.4,1,1,1h4v4c0,0.6,0.4,1,1,1h1c0.6,0,1-0.4,1-1V9h4c0.6,0,1-0.4,1-1V7c0-0.6-0.4-1-1-1H9V2c0-0.6-0.4-1-1-1H7z")
 :own-description-paragraphs (list "Hospitals that are supposed to deal with medical emergencies thanks to its medical personnel, medical equipment and emergency department. This layer includes hospitals where data does not tell whether there effectively is an emergency department or not.")
 :wiki-definitions (asplice "Hospital"
                            "Emergency ward entrance")
 :columns (list (quick-tag "name" "name")
                (quick-tag "street" "tags->'addr:street'")
                (quick-tag "housenumber" (dsl :quote "addr:housenumber") "house number")
                (quick-tag "phone")
                (quick-tag "email")
                (quick-tag "emergency" nil "information related to emergency"))
 :predicate (dsl '(" AND ")
                 (dsl '("(" " OR " ")")
                      "amenity='hospital'"
                      "tags->'healthcare'='hospital'")
                 (dsl '("(" " OR " ")")
                      "tags->'emergency' is null"
                      "tags->'emergency' in ('yes','y','emergency_ward_entrance')"))
 :user-facing-predicate "(amenity=hospital or healthcare=hospital) and emergency=(??? or 'yes' or 'y' or 'emergency_ward_entrance')"
 :renderer (tooltip-feature-addressable
             (let ((emergency (my-assoc "emergency" row)))
               (format stream "~@[<br>Emergency: ~A~]"
                       (cond ((string-equal "no" emergency) "no")
                             ((not (eql :null emergency)) emergency))))))

(register-layer
 :name "no_emergency_medicare"
 :category "Health"
 :user-facing-name "No emergency medical care"
 :map-symbol (map-symbol-svg :health "M5.5,7C4.1193,7,3,5.8807,3,4.5l0,0v-2C3,2.2239,3.2239,2,3.5,2H4c0.2761,0,0.5-0.2239,0.5-0.5S4.2761,1,4,1H3.5&#xA;&#x9;C2.6716,1,2,1.6716,2,2.5v2c0.0013,1.1466,0.5658,2.2195,1.51,2.87l0,0C4.4131,8.1662,4.9514,9.297,5,10.5C5,12.433,6.567,14,8.5,14&#xA;&#x9;s3.5-1.567,3.5-3.5V9.93c1.0695-0.2761,1.7126-1.367,1.4365-2.4365C13.1603,6.424,12.0695,5.7809,11,6.057&#xA;&#x9;C9.9305,6.3332,9.2874,7.424,9.5635,8.4935C9.7454,9.198,10.2955,9.7481,11,9.93v0.57c0,1.3807-1.1193,2.5-2.5,2.5S6,11.8807,6,10.5&#xA;&#x9;c0.0511-1.2045,0.5932-2.3356,1.5-3.13l0,0C8.4404,6.7172,9.001,5.6448,9,4.5v-2C9,1.6716,8.3284,1,7.5,1H7&#xA;&#x9;C6.7239,1,6.5,1.2239,6.5,1.5S6.7239,2,7,2h0.5C7.7761,2,8,2.2239,8,2.5v2l0,0C8,5.8807,6.8807,7,5.5,7 M11.5,9&#xA;&#x9;c-0.5523,0-1-0.4477-1-1s0.4477-1,1-1s1,0.4477,1,1S12.0523,9,11.5,9z")
 :own-description-paragraphs (list "Medical care when there is no emergency, i.e. medical attention requiring an appointment. This layer contains hospitals which explicitly aren't equipped for emergencies, clinics and doctor's offices.")
 :wiki-definitions (list "Clinic"
                         "Doctor's office")
 :columns (list (quick-tag "name" "name")
                (quick-tag "street" "tags->'addr:street'")
                (quick-tag "housenumber" (dsl :quote "addr:housenumber") "house number")
                (quick-tag "phone")
                (quick-tag "email")
                (quick-tag "openinghours" "tags->'opening_hours'" "opening hours"))
 :predicate (dsl '("(" " OR " ")")
                (dsl '(" AND ")
                     (dsl '("(" " OR " ")")
                          "amenity='hospital'"
                          "tags->'healthcare'='hospital'")
                     "tags->'emergency'='no'")
                "amenity='clinic'"
                "amenity='doctors'")
 :user-facing-predicate "((amenity=hospital or healthcare=hospital) and emergency=no) or amenity=('clinic' or 'doctors')"
 :renderer (tooltip-feature-addressable-opening-hours))

(register-layer
 :name "pharmacies"
 :category "Health"
 :user-facing-name "Pharmacies and medical supply"
 :map-symbol (map-symbol-svg :health "M9.5,4l1.07-1.54c0.0599,0.0046,0.1201,0.0046,0.18,0c0.6904-0.0004,1.2497-0.5603,1.2494-1.2506&#xA;&#x9;C11.999,0.519,11.4391-0.0404,10.7487-0.04C10.0584-0.0396,9.499,0.5203,9.4994,1.2106c0,0.0131,0.0002,0.0262,0.0006,0.0394&#xA;&#x9;c0,0,0,0.07,0,0.1L7,4H9.5z M12,6V5H3v1l1.5,3.5L3,13v1h9v-1l-1-3.5L12,6z M10,10H8v2H7v-2H5V9h2V7h1v2h2V10z")
 :own-description-paragraphs (list "Pharmacies / drug stores. Thematically related shops which sell medical supplies are included, too.")
 :wiki-definitions (list "Pharmacy"
                         "Medical supply store")
 :columns (list (quick-tag "name" "name")
                (quick-tag "street" "tags->'addr:street'")
                (quick-tag "housenumber" (dsl :quote "addr:housenumber") "house number")
                (quick-tag "phone")
                (quick-tag "email")
                (quick-tag "openinghours" "tags->'opening_hours'" "opening hours"))
 :predicate (dsl '(" OR ")
                "amenity='pharmacy'"
                "shop='pharmacy'"
                "shop='medical_supply'")
 :user-facing-predicate "amenity=pharmacy or shop=(pharmacy or medical_supply)"
 :renderer (tooltip-feature-addressable-opening-hours))

(register-layer
 :name "chemists"
 :category "Health"
 :user-facing-name "Chemists"
 :map-symbol (x-svg :xmlns "http://www.w3.org/2000/svg"
                    :width "100"
                    :height "100"
                    :viewBox "0 0 100 100"
                    (x-path :d "M 60.27,14.4 V 19 H 32.89 v -4.6 h 2.64 v -12.71 0 C 35.53,0.75 40.48,0 46.57,0 52.66,0 57.6,0.75 57.6,1.69 v 0 12.71 z"
                            :fill (map-symbol-color :shop))
                    (x-path :d "M 73.97,31.46 60.72,22.2 H 32.68 L 22.89,33.77 22,100 H 81.64 L 81.99,59.06 Z M 65.41,60.94 60.2,42.95 c -0.12,-8.57 4.53,-8.27 7.44,-6.15 2.02,1.5 2.62,4.12 2.62,4.12 l 2.41,19.82 c -3.23,6.64 -7.26,0.2 -7.26,0.2 z"
                            :fill (map-symbol-color :shop)))
 :own-description-paragraphs (list "Retail stores offering personal healthcare items, household products and cosmetics.")
 :wiki-definitions nil
 :columns (list (quick-tag "name" "name")
                (quick-tag "street" "tags->'addr:street'")
                (quick-tag "housenumber" (dsl :quote "addr:housenumber") "house number")
                (quick-tag "phone")
                (quick-tag "email")
                (quick-tag "openinghours" "tags->'opening_hours'" "opening hours"))
 :predicate "shop='chemist'"
 :user-facing-predicate "shop=chemist"
 :renderer (tooltip-feature-addressable-opening-hours))

(register-layer
 :name "police_stations"
 :category "Security"
 :user-facing-name "Police stations and customs"
 :map-symbol (map-symbol-svg :amenity "M5.5,1L6,2h5l0.5-1H5.5z M6,2.5v1.25c0,0,0,2.75,2.5,2.75S11,3.75,11,3.75V2.5H6z M1.9844,3.9863&#xA;&#x9;C1.4329,3.9949,0.9924,4.4485,1,5v4c-0.0001,0.6398,0.5922,1.1152,1.2168,0.9766L5,9.3574V14l5.8789-6.9297&#xA;&#x9;C10.7391,7.0294,10.5947,7,10.4414,7H6.5L3,7.7539V5C3.0077,4.4362,2.5481,3.9775,1.9844,3.9863z M11.748,7.7109L6.4121,14H12&#xA;&#x9;V8.5586C12,8.2451,11.9061,7.9548,11.748,7.7109z")
 :own-description-paragraphs (list "Police stations which typically have a reception desk for the general public. Thematically related customs offices are included, too.")
 :wiki-definitions (asplice "Police station")
 :columns (list (quick-tag "name" "name")
                (quick-tag "street" "tags->'addr:street'")
                (quick-tag "housenumber" (dsl :quote "addr:housenumber") "house number")
                (quick-tag "phone")
                (quick-tag "email"))
 :predicate (dsl '("(" " OR " ")")
                "amenity='police'"
                "tags->'government'='customs'")
 :user-facing-predicate "amenity=police or government=customs"
 :renderer (tooltip-feature-addressable))

#|
The inclusion of fire brigades is not very useful since /they/ drive to humans in emergency. Skipping ...
|#

(register-layer
 :name "libraries"
 :category "Education"
 :user-facing-name "Libraries"
 :map-symbol (map-symbol-svg :amenity "M1.0819,9.9388C0.9871,9.867,1.0007,9.7479,1.0007,9.7479L1.5259,3.5c0,0,0.0082-0.0688,0.0388-0.104&#xA;&#x9;C1.584,3.374,1.6084,3.342,1.6544,3.3232C2.1826,3.1072,5.0537,1.5519,6.5,3c0.2397,0.2777,0.4999,0.6876,0.4999,1v5.2879&#xA;&#x9;c0,0,0.0062,0.1122-0.0953,0.1801c-0.0239,0.016-0.124,0.0616-0.242,0.0026c-2.2253-1.1134-4.711,0.1546-5.3381,0.4871&#xA;&#x9;C1.1987,10.0244,1.1006,9.9531,1.0819,9.9388z M13.6754,9.9577c-0.6271-0.3325-3.1128-1.6005-5.3381-0.4871&#xA;&#x9;c-0.118,0.059-0.2181,0.0134-0.242-0.0026C7.9939,9.4001,8.0001,9.2879,8.0001,9.2879V4c0-0.3124,0.2602-0.7223,0.4999-1&#xA;&#x9;c1.4463-1.4481,4.2991,0.1071,4.8273,0.3232c0.046,0.0188,0.0704,0.0508,0.0897,0.0728C13.4476,3.4312,13.4558,3.5,13.4558,3.5&#xA;&#x9;l0.5435,6.2479c0,0,0.0136,0.1191-0.0812,0.1909C13.8994,9.9531,13.8013,10.0244,13.6754,9.9577z M8.8647,12.6863&#xA;&#x9;c0.0352-0.0085,0.0964-0.0443,0.1179-0.0775c0.0236-0.0364,0.0378-0.0617,0.0423-0.1088c0.0495-0.9379,1.6245-1.8119,4.6477-0.0298&#xA;&#x9;c0.0775,0.0441,0.1666,0.0396,0.2425-0.0155C14.0014,12.392,14,12.2859,14,12.2859v-0.5542c0,0,0.0003-0.0764-0.0272-0.1184&#xA;&#x9;c-0.0205-0.0312-0.0476-0.0643-0.0926-0.0858c-2.0254-1.3145-4.5858-1.8972-5.8854-0.1592&#xA;&#x9;c-0.0181,0.0423-0.0353,0.0613-0.0728,0.0905C7.8654,11.5028,7.7964,11.5,7.7964,11.5H7.2109c0,0-0.069,0.0028-0.1256-0.0412&#xA;&#x9;c-0.0375-0.0292-0.0547-0.0482-0.0728-0.0905c-1.2996-1.738-3.86-1.1828-5.8854,0.1317c-0.045,0.0215-0.0721,0.0546-0.0926,0.0858&#xA;&#x9;c-0.0275,0.042-0.0272,0.1184-0.0272,0.1184v0.5542c0,0-0.0014,0.1061,0.0849,0.1688c0.0759,0.0551,0.165,0.0596,0.2425,0.0155&#xA;&#x9;c3.0232-1.7821,4.5982-0.8806,4.6477,0.0573c0.0045,0.0471,0.0187,0.0724,0.0423,0.1088c0.0215,0.0332,0.0827,0.069,0.1179,0.0775&#xA;&#x9;C6.8645,12.8656,7.9112,12.9363,8.8647,12.6863z")
 :own-description-paragraphs (list "Accessible collections of books and other information carriers, often housed in buildings which additionally provide quiet study areas. Includes public bookcases or little free libraries, where such information carriers can be exchanged for free.")
 :wiki-definitions nil
 :columns (list (quick-tag "name" "name")
                (quick-tag "street" "tags->'addr:street'")
                (quick-tag "housenumber" (dsl :quote "addr:housenumber") "house number")
                (quick-tag "phone")
                (quick-tag "email")
                (quick-tag "openinghours" "tags->'opening_hours'" "opening hours"))
 :predicate (dsl '("(" " OR " ")")
                 "amenity='library'"
                 "amenity='public_bookcase'")
 :user-facing-predicate "amenity=(library or public_bookcase)"
 :renderer (tooltip-feature-addressable-opening-hours))

(register-layer
 :name "public_transport_stops"
 :category "Infrastructure"
 :user-facing-name "Public transport stops"
 :map-symbol (map-symbol-svg :transport "M2 3C2 1.9 2.9 1 4 1H11C12.1 1 13 1.9 13 3V11C13 12 12 12 12 12V13C12 13.55 11.55 14 11 14C10.45 14 10 13.55 10 13V12H5V13C5 13.55 4.55 14 4 14C3.45 14 3 13.55 3 13V12C2 12 2 11 2 11V3ZM3.5 4C3.22 4 3 4.22 3 4.5V7.5C3 7.78 3.22 8 3.5 8H11.5C11.78 8 12 7.78 12 7.5V4.5C12 4.22 11.78 4 11.5 4H3.5ZM4 9C3.45 9 3 9.45 3 10C3 10.55 3.45 11 4 11C4.55 11 5 10.55 5 10C5 9.45 4.55 9 4 9ZM11 9C10.45 9 10 9.45 10 10C10 10.55 10.45 11 11 11C11.55 11 12 10.55 12 10C12 9.45 11.55 9 11 9ZM4 2.5C4 2.78 4.22 3 4.5 3H10.5C10.78 3 11 2.78 11 2.5C11 2.22 10.78 2 10.5 2H4.5C4.22 2 4 2.22 4 2.5Z")
 :own-description-paragraphs (list "Stations served by public transport. Considered vehicles are busses, trains, trams, subways, ferries, planes and cable cars (gondolas, ski lifts).")
 :wiki-definitions nil
 :columns (list (quick-tag "name" "name"))
 ;;TODO: obviously not synchronized to openstreetmap-carto/project.mml.
 :predicate (dsl '(" OR ")
                 "highway='station'"
                 "highway='platform'"
                 "railway='station'"
                 "railway='halt'"
                 "railway='tram_stop'"
                 "railway='subway_entrance'"
                 "amenity='ferry_terminal'"
                 "aeroway='aerodrome'"
                 "aeroway='heliport'"
                 "aerialway='station'")
 :user-facing-predicate "highway=station or highway=platform or railway=station or railway=halt or railway=tram_stop or railway=subway_entrance or amenity=ferry_terminal or aeroway=aerodrome or aeroway=heliport or aerialway=station"
 :renderer (tooltip-feature
             (let ((name (my-assoc "name" row)))
               (format stream "\"~A\""
                       (if (eql :null name)
                           "???"
                           (escape-js name))))))

(register-layer
 :name "fuel"
 :category "Infrastructure"
 :user-facing-name "Fuel"
 :map-symbol (map-symbol-svg :transport "m14 6v5.5c0 .2761-.2239.5-.5.5s-.5-.2239-.5-.5v-2c0-.8284-.6716-1.5-1.5-1.5h-1.5v-6c0-.5523-.4477-1-1-1h-6c-.5523 0-1 .4477-1 1v11c0 .5523.4477 1 1 1h6c.5523 0 1-.4477 1-1v-4h1.5c.2761 0 .5.2239.5.5v2c0 .8284.6716 1.5 1.5 1.5s1.5-.6716 1.5-1.5v-6.5c0-.5523-.4477-1-1-1v-1.51c-.0054-.2722-.2277-.4901-.5-.49-.2816.0047-.5062.2367-.5015.5184.0002.0105.0007.0211.0015.0316v2.45c0 .5523.4477 1 1 1s1-.4477 1-1-.4477-1-1-1zm-5 .5c0 .2761-.2239.5-.5.5h-5c-.2761 0-.5-.2239-.5-.5v-3c0-.2761.2239-.5.5-.5h5c.2761 0 .5.2239.5.5z")
 :own-description-paragraphs (list "Filling stations selling fuel that is typically delivered through gas pumps.")
 :wiki-definitions nil
 :columns (list (quick-tag "name" "name")
                (quick-tag "street" "tags->'addr:street'")
                (quick-tag "housenumber" (dsl :quote "addr:housenumber") "house number")
                (quick-tag "phone")
                (quick-tag "email")
                (quick-tag "openinghours" "tags->'opening_hours'" "opening hours"))
 :predicate "amenity='fuel'"
 :user-facing-predicate "amenity=fuel"
 :renderer (tooltip-feature-addressable-opening-hours))

(register-layer
 :name "drinking_water"
 :category "Infrastructure"
 :user-facing-name "Drinking water"
 :map-symbol (map-symbol-svg :amenity "M6,1A2,2,0,0,0,4,3V6.5a.5.5,0,0,0,.5.5h2A.5.5,0,0,0,7,6.5v-2A.5.5,0,0,1,7.5,4H14V1ZM7,15H4a.5.5,0,0,1-.48-.38L2,8.62a.5.5,0,0,1,.365-.606A.558.558,0,0,1,2.5,8h6a.5.5,0,0,1,.514.485A.47.47,0,0,1,9,8.62l-1.5,6A.5.5,0,0,1,7,15ZM3.65,11H7.36l.5-2H3.14Z")
 :own-description-paragraphs (list "Access points which provide drinking water. Includes places which provide large quantities.")
 :wiki-definitions nil
 :columns (list (quick-tag "name" "name"))
 :predicate (dsl '("(" " OR " ")")
                "amenity='drinking_water'"
                "amenity='water_point'")
 :user-facing-predicate "amenity=(drinking_water or water_point)<br><br>TODO: include water_point=yes."
 :renderer (tooltip-feature
             #|(let ((name (my-assoc "name" row)))
             (format stream "\"~A\""
                       (if (eql :null name)
                           "???"
             (escape-js name))))|#
             (format stream "\"W\"")))

(register-layer
 :name "toilets"
 :category "Infrastructure"
 :user-facing-name "Toilets"
 :map-symbol (map-symbol-svg :amenity "M3 1.5a1.5 1.5 0 1 0 3 0 1.5 1.5 0 0 0-3 0ZM11.5 0a1.5 1.5 0 1 1 0 3 1.5 1.5 0 0 1 0-3ZM3.29 4a1 1 0 0 0-.868.504L.566 7.752a.5.5 0 1 0 .868.496l1.412-2.472A345.048 345.048 0 0 0 1 11h2v2.5a.5.5 0 0 0 1 0V11h1v2.5a.5.5 0 0 0 1 0V11h2L6.103 5.687l1.463 2.561a.5.5 0 1 0 .868-.496L6.578 4.504A1 1 0 0 0 5.71 4H3.29ZM9 4.5a.5.5 0 0 1 .5-.5h4a.5.5 0 0 1 .5.5v5a.5.5 0 0 1-1 0v4a.5.5 0 0 1-1 0v-4h-1v4a.5.5 0 0 1-1 0v-4a.5.5 0 0 1-1 0v-5Z")
 :own-description-paragraphs (list "Toilets open to the public. Access rules may apply.")
 :wiki-definitions nil
 :columns (list (quick-tag "name" "name")
                (quick-tag "street" "tags->'addr:street'")
                (quick-tag "housenumber" (dsl :quote "addr:housenumber") "house number")
                (quick-tag "phone")
                (quick-tag "email")
                (quick-tag "openinghours" "tags->'opening_hours'" "opening hours")
                (quick-tag "\"access\"" "coalesce(tags->'access', tags->'toilets:access')" "access rules"))
 :predicate "(amenity='toilets' or amenity='toilet' or amenity='wc' or building='toilets' or tags->'toilets'='yes') and (tags->'toilets' is null or tags->'toilets'<>'no')"
 :user-facing-predicate "(amenity=toilets or amenity=toilet or amentiy=wc or building=toilets or toilets=yes) and toilets=(??? or (not no))"
 :renderer (tooltip-feature-addressable-opening-hours
             (let ((access (my-assoc "access" row)))
               (unless (eql :null access)
                 (format stream "<br>Access: ~A" access)))))

(register-layer
 :name "shower"
 :category "Infrastructure"
 :user-facing-name "Showers"
 :map-symbol (map-symbol-svg :amenity
                             "m 3.4885566,14.048049 c 1.1503612,0 2.4925009,-0.713575 2.4925009,-2.529939 0,-1.232497 -1.9173203,-3.9570435 -2.4925009,-4.6706189 -0.5112894,0.7135754 -2.49250083,3.3732229 -2.49250083,4.6706189 0,1.816364 1.34213973,2.529939 2.49250083,2.529939 z"
                             "m 11.232889,12.393981 c 1.150361,0 2.492501,-0.713575 2.492501,-2.5299395 0,-1.232497 -1.91732,-3.9570431 -2.492501,-4.6706185 -0.51129,0.7135754 -2.4925011,3.3732225 -2.4925011,4.6706185 0,1.8163645 1.3421401,2.5299395 2.4925011,2.5299395 z"
                             "m 7.0003437,7.4253908 c 1.150361,0 2.492501,-0.713575 2.492501,-2.5299392 0,-1.232497 -1.91732,-3.95704354 -2.492501,-4.67061894 -0.5112892,0.7135754 -2.4925006,3.37322294 -2.4925006,4.67061894 0,1.8163642 1.3421397,2.5299392 2.4925006,2.5299392 z")
 :own-description-paragraphs (list "Public showers.")
 :wiki-definitions nil
 :columns (list (quick-tag "name" "name")
                (quick-tag "street" "tags->'addr:street'")
                (quick-tag "housenumber" (dsl :quote "addr:housenumber") "house number")
                (quick-tag "phone")
                (quick-tag "email")
                (quick-tag "openinghours" "tags->'opening_hours'" "opening hours"))
 :predicate "amenity='shower'"
 :user-facing-predicate "amenity=shower"
 :renderer (tooltip-feature-addressable-opening-hours))

(register-layer
 :name "laundries_lavoirs"
 :category "Infrastructure"
 :user-facing-name "Laundries and lavoirs"
 :map-symbol (x-svg :xmlns "http://www.w3.org/2000/svg"
                    :width "50"
                    :height "50"
                    :viewBox "0 0 50 50"
                    (x-path :d "M6 11v33.74c0 1.47 1.24 3.26 2.76 3.26h33.22c1.52 0 3.02 -1.79 3.02 -3.26v-33.74h-39zm19.46 26.78c-5.86 0 -10.61 -4.59 -10.61 -10.26s4.75 -10.26 10.61 -10.26s10.61 4.59 10.61 10.26c0 5.67 -4.75 10.26 -10.61 10.26zm16.52 -36.78h-33.22c-1.52 0 -2.76 1.03 -2.76 2.51v5.5h39v-5.49c0 -1.47 -1.5 -2.5 -3.02 -2.5zm-22.98 6h-11v-4h11v4zm19.15 -0.28c-1.25 0 -2.26 -0.98 -2.26 -2.19c0 -1.21 1.01 -2.19 2.26 -2.19s2.26 0.98 2.26 2.19c0 1.21 -1.01 2.19 -2.26 2.19z"
                            :fill (map-symbol-color :shop)))
 :own-description-paragraphs (list "Laundry facilities where it is possible to wash and dry clothes for a fee. Thematically related dry cleaning stores and lavoirs are included, too.")
 :wiki-definitions nil
 :columns (list (quick-tag "name" "name")
                (quick-tag "street" "tags->'addr:street'")
                (quick-tag "housenumber" (dsl :quote "addr:housenumber") "house number")
                (quick-tag "phone")
                (quick-tag "email")
                (quick-tag "openinghours" "tags->'opening_hours'" "opening hours"))
 :predicate (dsl '("(" " OR " ")")
                 "shop='laundry'"
                 "shop='dry_cleaning'"
                 "amenity='lavoir'")
 :user-facing-predicate "shop=laundry or shop=dry_cleaning or amenity=lavoir"
 :renderer (tooltip-feature-addressable-opening-hours))

(register-layer
 :name "shelter"
 :category "Infrastructure"
 :user-facing-name "Shelters"
 :map-symbol (map-symbol-svg :shelter "M13 2L1 6V8L2 7.66667V13H12V11H4V7L13 4V2Z")
 :own-description-paragraphs (list "Shelters protect you from wind and weather.")
 :wiki-definitions nil
 :columns (list (quick-tag "name" "name"))
 :predicate "amenity='shelter'"
 :user-facing-predicate "amenity=shelter"
 :renderer (tooltip-feature
             (format stream "\"S\"")))

(register-layer
 :name "social_facility"
 :category "Social"
 :user-facing-name "Social facilities"
 :map-symbol (map-symbol-svg :amenity
                             "M 3.715214,13.063947 V 8.4199299 C 3.715214,7.9272554 3.5194965,7.454773 3.1711395,7.1063974 2.8227639,6.7580404 2.3502815,6.5623229 1.857607,6.5623229 c -0.4926745,0 -0.96515692,0.1957175 -1.31353252,0.5440745 C 0.1957175,7.454773 0,7.9272554 0,8.4199299 V 13.063947 Z M 0,3.7759124 C 0,3.4085149 0.1089487,3.0493652 0.31306248,2.7438817 0.51717638,2.4383982 0.80729738,2.2003087 1.146738,2.0597064 1.4861599,1.9191042 1.8596689,1.8823236 2.2200075,1.9540086 2.5803462,2.0256751 2.9113346,2.2025936 3.1711395,2.4623799 3.4309258,2.7221848 3.6078443,3.0531732 3.6795294,3.4135119 3.7511958,3.7738505 3.7144152,4.1473595 3.573813,4.4867815 3.4332107,4.826222 3.1951212,5.116343 2.8896377,5.3204569 2.5841543,5.5245708 2.2250045,5.6335194 1.857607,5.6335194 1.3649325,5.6335194 0.89245008,5.4378019 0.54407448,5.0894449 0.1957175,4.7410693 0,4.2685869 0,3.7759124 Z"
                             "M 15,13.063947 V 8.4199298 C 15,7.9272558 14.804282,7.4547731 14.455925,7.1063975 14.10755,6.7580405 13.635067,6.562323 13.142393,6.562323 c -0.492675,0 -0.965157,0.1957175 -1.313533,0.5440745 -0.348357,0.3483756 -0.544074,0.8208583 -0.544074,1.3135323 V 13.063947 Z M 11.284786,3.7759125 c 0,-0.3673975 0.108948,-0.7265472 0.313062,-1.0320307 0.204114,-0.3054835 0.494235,-0.543573 0.833676,-0.6841753 0.339422,-0.1406022 0.712931,-0.1773828 1.073269,-0.1056978 0.360339,0.071667 0.691327,0.248585 0.951132,0.5083713 0.259786,0.2598049 0.436705,0.5907933 0.50839,0.951132 0.07167,0.3603386 0.03489,0.7338476 -0.105716,1.0732696 -0.140603,0.3394405 -0.378692,0.6295615 -0.684176,0.8336754 -0.305483,0.2041139 -0.664633,0.3130625 -1.03203,0.3130625 -0.492675,0 -0.965157,-0.1957175 -1.313533,-0.5440745 C 11.480503,4.7410694 11.284786,4.268587 11.284786,3.7759125 Z"
                             "M 9.3585791,13.063947 V 8.4199288 c 0,-0.4926745 -0.1957175,-0.9651569 -0.5440745,-1.3135325 C 8.466129,6.7580393 7.9936466,6.5623218 7.5009721,6.5623218 c -0.4926745,0 -0.9651569,0.1957175 -1.3135325,0.5440745 -0.348357,0.3483756 -0.5440745,0.820858 -0.5440745,1.3135325 V 13.063947 Z M 5.6433651,3.7759113 c 0,-0.3673975 0.1089487,-0.7265472 0.3130625,-1.0320307 C 6.1605415,2.4383971 6.4506625,2.2003076 6.7901031,2.0597053 7.129525,1.9191031 7.503034,1.8823225 7.8633726,1.9540075 c 0.3603387,0.071667 0.6913271,0.248585 0.951132,0.5083713 0.2597863,0.2598049 0.4367048,0.5907933 0.5083899,0.951132 0.071666,0.3603386 0.034886,0.7338476 -0.1057164,1.0732696 C 9.0765758,4.8262209 8.8384863,5.1163419 8.5330028,5.3204558 8.2275194,5.5245697 7.8683696,5.6335183 7.5009721,5.6335183 7.0082976,5.6335183 6.5358152,5.4378008 6.1874396,5.0894438 5.8390826,4.7410682 5.6433651,4.2685858 5.6433651,3.7759113 Z")
 :own-description-paragraphs (list "Social facilities are, in broad terms, focussed on the well-being of humans.")
 :wiki-definitions (list "Social facility")
 :columns (list (quick-tag "name" "name")
                (quick-tag "street" "tags->'addr:street'")
                (quick-tag "housenumber" (dsl :quote "addr:housenumber") "house number")
                (quick-tag "phone")
                (quick-tag "email")
                (quick-tag "openinghours" "tags->'opening_hours'" "opening hours")
                (quick-tag "facility_type" "tags->'social_facility'" "facility type"))
 :predicate "amenity='social_facility'"
 :user-facing-predicate "amenity=social_facility"
 :renderer (tooltip-feature-addressable-opening-hours
             (let ((facility-type (my-assoc "facility_type" row)))
               (format stream "~@[<br>Type: ~A~]"
                       (unless (eql :null facility-type)
                         (escape-js facility-type))))))

