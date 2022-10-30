;;;; TOO (The Offline Oriented) creates ZIM files with offline available maps
;;;; Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>
;;;; 
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :too)

;;;; Side note: reprojection is very amenable to an elaborate CLOS example. ... Will there ever be an ISBN for "A spatial case for CLOS"?


;;;; POINT

(defclass point () ())

(defclass point-4326 (point)
  ((lat :accessor lat :initarg :lat :initform nil)
   (lon :accessor lon :initarg :lon :initform nil))
  (:documentation "A point in EPSG:4326 (WGS84) with coordinates in latitude/longitude."))

(defclass point-3857 (point)
  ((x :accessor x :initarg :x :initform nil)
   (y :accessor y :initarg :y :initform nil))
  (:documentation "A point in EPSG:3857 (spherical mercator). X=0 Y=0 is lat=0 lon=0."))

(defclass point-renderd (point-3857)
  ((z :accessor z :initarg :z :initform nil :documentation "Zoom level, [0..20]."))
  (:documentation "A tile grid offset as used by renderd. Y starts from the north pole. X and Y are dependent on Z."))
(make-instance 'point-renderd :x 123 :y 456 :z 3)

(defgeneric reproject (from to &key)
  (:documentation "Reproject a point or a MBR to a new coordinate system. FROM is expected to be a subtype of POINT or MBR.")
  (:method ((from point-4326) (to (eql :point-4326)) &key)
    from)
  (:method ((from point-3857) (to (eql :point-3857)) &key)
    from)
  (:method ((from point-renderd) (to (eql :point-renderd)) &key)
    from))

;;Adapted from libosmium_2.18.0/include/osmium/geom/mercator_projection.hpp, see legal/libosmium.txt.
(defmethod reproject ((from point-4326) (to (eql :point-3857)) &key)
  "Intentionally allows contagion - good enough for TOO's mapping tasks."
  ;;(assert (<= -85.0511288 (lat from) 85.0511288))
  (let ((earth-radius-for-epsg3857 6378137.0))
    (flet ((deg2rad (deg)
             "Degree to radian conversion."
             (/ (* deg pi) 180))
           (on-earth (value) (* earth-radius-for-epsg3857 value)))
      (make-instance 'point-3857
                     :y (on-earth (let ((temp (tan (+ (/ pi 4)
                                                      (/ (deg2rad (lat from)) 2)))))
                                    (if (= 0 temp)
                                        temp
                                        (log temp))))
                     :x (on-earth (deg2rad (lon from)))))))
(reproject (make-instance 'point-4326 :lat 0 :lon 0) :point-3857)
(reproject (make-instance 'point-4326 :lat -10 :lon -10) :point-3857)
(reproject (make-instance 'point-4326 :lat 85.0511 :lon 180) :point-3857)
(reproject (make-instance 'point-4326 :lat 1.23 :lon 4.56) :point-3857)

(defun zoom-offset (&optional (tile-size 256))
  "0 if tile-size 256, -1 if tile-size 512, -2 if tile-size 1024 ..."
  (- (round (log (/ tile-size 256) 2))))

(defun as-256px-zoom (zoom tile-size-of-given-zoom)
  "Convert ZOOM to 256px zoom."
  (- zoom (zoom-offset tile-size-of-given-zoom)))
;;Zoom 12 at tile size 1024 is 256px-zoom 14.
(as-256px-zoom 12 1024)

(defun as-non-256px-zoom (256px-zoom tile-size-of-target-zoom)
  "Convert 256PX-ZOOM to the equivalent zoom at TILE-SIZE-OF-TARGET-ZOOM."
  (+ 256px-zoom (zoom-offset tile-size-of-target-zoom)))
(as-non-256px-zoom 14 1024)

(defmethod reproject ((from point-3857) (to (eql :point-renderd)) &key (zoom 20))
  "Given a point in EPSG:3857, return the tile coordinates that renderd uses."
  (let ((tiles-on-axis (ash 1 zoom))
        ;;Adapted from libosmium_2.18.0/include/osmium/geom/mercator_projection.hpp, see legal/libosmium.txt.
        (max-coordinate-for-epsg3857 20037508.34))
    (flet ((zoomed-offset (x-or-y)
             (truncate (* (/ (+ max-coordinate-for-epsg3857 x-or-y)
                             (* 2 max-coordinate-for-epsg3857))
                          tiles-on-axis))))
      (make-instance 'point-renderd
                     :x (zoomed-offset (x from))
                     ;;Tile Map Service Specification has easting/northing, but OpenStreetMap/Google start at the upper left corner. → Flip the axis.
                     :y (zoomed-offset (- (y from)))
                     :z zoom))))

(defmethod reproject ((from point-4326) (to (eql :point-renderd)) &key (zoom 20))
  "WGS84 → renderd conversion."
  (reproject (reproject from :point-3857) :point-renderd :zoom zoom))
(reproject (make-instance 'point-4326 :lat 1.23 :lon 4.56) :point-renderd :zoom 10)
(reproject (make-instance 'point-4326 :lat -1.23 :lon 4.56) :point-renderd :zoom 10)


;;;; MBR

(defclass mbr ()
  ((ll :accessor ll :initarg :ll :initform nil :documentation "Lower left corner.")
   (ur :accessor ur :initarg :ur :initform nil :documentation "Upper right corner."))
  (:documentation "A minimum bounding rectangle. Don't cross the antimeridian."))

(defclass mbr-4326 (mbr) ()
  (:documentation "MBR in EPSG:4326 (WGS 84)."))

(defmethod reproject ((from mbr-4326) (to (eql :mbr-4326)) &key) from)

(defmethod initialize-instance :around ((object mbr-4326) &key ll ur ll-lat ll-lon ur-lat ur-lon)
  "Quick helper for convenient `mbr-4326 construction.
   Official :LL :UR initargs have precedence over LL-LAT LL-LON UR-LAT UR-LON."
  (call-next-method object :ll (or ll (make-instance 'point-4326 :lat ll-lat :lon ll-lon))
                           :ur (or ur (make-instance 'point-4326 :lat ur-lat :lon ur-lon))))

(defmethod ll-lat ((mbr mbr-4326))
  "Quick helper allowing for flat access paths to MBR's values."
  (lat (ll mbr)))

(defmethod ll-lon ((mbr mbr-4326))
  "Quick helper allowing for flat access paths to MBR's values."
  (lon (ll mbr)))

(defmethod ur-lat ((mbr mbr-4326))
  "Quick helper allowing for flat access paths to MBR's values."
  (lat (ur mbr)))

(defmethod ur-lon ((mbr mbr-4326))
  "Quick helper allowing for flat access paths to MBR's values."
  (lon (ur mbr)))

(defparameter *3857-aware-earth* (make-instance 'mbr-4326
                                                :ll-lat -85.0511 :ll-lon -180
                                                :ur-lat 85.0511 :ur-lon 179.9999)
  "The maximally supported values of the EPSG 3857 projection, with minor adjustments for edge cases that are inherent in map tile calculations.")

(defmethod clamp-mbr ((mbr mbr-4326) &key (clipping-mbr *3857-aware-earth*))
  "Clamp the MBR to CLIPPING-MBR."
  (make-instance 'mbr-4326
                 :ll-lat (max (ll-lat clipping-mbr) (ll-lat mbr))
                 :ll-lon (max (ll-lon clipping-mbr) (ll-lon mbr))
                 :ur-lat (min (ur-lat clipping-mbr) (ur-lat mbr))
                 :ur-lon (min (ur-lon clipping-mbr) (ur-lon mbr))))

(defmethod equal-mbr? ((mbr0 mbr-4326) (mbr1 mbr-4326) &key (test #'equal))
  "Are MBR0 MBR1 equal according to TEST?"
  (and (funcall test (ll-lat mbr0) (ll-lat mbr1))
       (funcall test (ll-lon mbr0) (ll-lon mbr1))
       (funcall test (ur-lat mbr0) (ur-lat mbr1))
       (funcall test (ur-lon mbr0) (ur-lon mbr1))))
(equal-mbr? *3857-aware-earth* (make-instance 'mbr-4326
                                              :ll-lat -85.0511 :ll-lon -180
                                              :ur-lat 85.0511 :ur-lon 179.9999))

(defclass mbr-renderd (mbr) ()
  (:documentation "A MBR with LL UR being instances of `point-renderd."))

(defmethod reproject ((from mbr-renderd) (to (eql :mbr-renderd)) &key) from)

(defparameter *max-zoom-level-without-region-limits* 6
  "The maximum map tile zoom level for which region limits are ignored.")

(defmethod reproject ((from mbr-4326) (to (eql :mbr-renderd)) &key (zoom 20) filter-on-low-zoom-levels-too?)
  "Reproject FROM to a `mbr-renderd.
   ZOOM in interval [0..20].
   FILTER-ON-LOW-ZOOM-LEVELS-TOO? NIL logically overrides FROM to a global MBR if (<= ZOOM *max-zoom-level-without-region-limits*)."
  (let* ((possibly-global-mbr (if (and (<= zoom *max-zoom-level-without-region-limits*)
                                       (not filter-on-low-zoom-levels-too?))
                                  *3857-aware-earth*
                                  from))
         (ll (reproject (ll possibly-global-mbr) :point-renderd :zoom zoom))
         (ur (reproject (ur possibly-global-mbr) :point-renderd :zoom zoom)))
    (make-instance 'mbr-renderd
                   ;;Reversing again since min of y value has to be the smaller value.
                   :ll (make-instance 'point-renderd :x (x ll) :y (y ur) :z zoom)
                   :ur (make-instance 'point-renderd :x (x ur) :y (y ll) :z zoom))))
(reproject (make-instance 'mbr-4326 :ll-lat 12 :ll-lon 34 :ur-lat 56 :ur-lon 78) :mbr-renderd :zoom 10)
(reproject (make-instance 'mbr-4326 :ll-lat -12 :ll-lon -34 :ur-lat -11 :ur-lon -33) :mbr-renderd :zoom 10)
;;(ash 1 20) is 1048576.
(reproject *3857-aware-earth* :mbr-renderd :zoom 20)


;;;; Output helpers

(defun write-tile-url (&key stream (url-base "http://127.0.0.1/osm1024/") zoom x y)
  "Write the url (given by URL-BASE ZOOM X Y) to STREAM"
  (declare (inline))
  (princ url-base stream)
  (princ zoom stream)
  (princ #\/ stream)
  (princ x stream)
  (princ #\/ stream)
  (princ y stream)
  (princ ".png" stream)
  (princ #\Newline stream))

(defun wget-tiles-in-mbr (&key
                            (url-base "http://127.0.0.1/osm256/")
                            (mbr *3857-aware-earth*)
                            (zoom-min 0)
                            (zoom-max 20)
                            (zoom-delta 1))
  "Write a list of renderd tile URLs intersecting the given MBR (an instance of `mbr-4326) to a file whose file path is returned.
   In /dev/shm, use wget --quiet --force-directories --no-host-directories --cut-dirs=1 --input-file=temp --output-document=/dev/null for a local tile cache in metatile files.
   ZOOM-MIN ZOOM-MAX are for 256px*256px tiles."
  (let ((outfile "/dev/shm/temp"))
    (with-open-file (stream outfile :direction :output :if-exists :supersede :if-does-not-exist :create)
      (loop for zoom from zoom-min upto zoom-max by zoom-delta
            do (let ((my-mbr (reproject mbr :mbr-renderd :zoom zoom)))
                 ;;THINK: Hilbert tree for an improved cache hit ratio? → renderd uses metatiles, which sort of does that job already. Additionally, huge tiles are used in the ZIM file.
                 (loop for x from (x (ll my-mbr)) upto (x (ur my-mbr))
                       do (loop for y from (y (ll my-mbr)) upto (y (ur my-mbr))
                                do (write-tile-url :stream stream :url-base url-base :zoom zoom :x x :y y))))))
    outfile))
(wget-tiles-in-mbr :zoom-min 0 :zoom-max 3)

(defun distance (x1 y1 x2 y2)
  "The distance between X1 Y1 and X2 Y2."
  (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))
(distance 0 0 3 1)

(defun point-buffer (&key x y buffer zoom)
  "All points lying within BUFFER distance from X Y, restricting the calculations to the renderd domain."
  (let ((real-zoom (ash 1 zoom)))
    (flet ((shift-away (value-here)
             "Shift VALUE-HERE into an area where cartesian calculations are undisturbed."
             (+ value-here real-zoom))
           (shift-back (value-there)
             "Shift VALUE-THERE back into a valid renderd domain."
             (rem value-there real-zoom)))
      (let* ((x-shifted (shift-away x))
             (y-shifted (shift-away y))
             (xmin (- x-shifted buffer))
             (xmax (+ x-shifted buffer))
             (ymin (- y-shifted buffer))
             (ymax (+ y-shifted buffer)))
        (loop for myx from xmin upto xmax
              append (loop for myy from ymin upto ymax
                           when (<= (distance (shift-away x) (shift-away y) myx myy) buffer)
                             collect (list (shift-back myx) (shift-back myy))))))))
(point-buffer :x 1023 :y 1023 :buffer 1 :zoom 10)

(defun wget-tiles-to-point (&key
                              (path "/dev/shm/temp")
                              (url-base "http://127.0.0.1/osm1024/")
                              (position (make-instance 'point-4326 :lat 0 :lon 0))
                              (tile-buffer 1)
                              (zoom-min 0)
                              (zoom-max 20)
                              (tile-size 1024))
  "Write a list of tile URLs around the point at POSITION to PATH.
   Will result in a circle approximation."
  (with-open-file (stream path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (loop for zoom from zoom-min upto zoom-max
          do (loop for point in (let ((projected (reproject position :point-renderd :zoom zoom)))
                                  (point-buffer :x (x projected)
                                                :y (y projected)
                                                :buffer tile-buffer
                                                :zoom zoom))
                   do (write-tile-url :stream stream :zoom (max 0 (+ zoom (zoom-offset tile-size))) :x (first point) :y (second point) :url-base url-base)))))
(let ((mysize 2048))
  (wget-tiles-to-point :position (make-instance 'point-4326 :lat 12 :lon 34) :tile-buffer 0 :zoom-min 0 :zoom-max 14
                       :url-base (format nil "http://127.0.0.1/osm~A/" mysize)))


;;;; Deduplicate empty tiles.
;;;; CACHE CONTROL: the values depend on real rendered tiles and may change for tile sizes other than 1024px.
;;;; THINK: While the empty tiles catch most cases, it would be nice to deduplicate *ALL* tiles. This would require hashes or n² bit-by-bit comparisons.
;;;;         → Not worth the disk costs.

(defconstant +size-of-empty-tiles+ 222)

(defparameter *identifying-parts-of-empty-tiles*
  (flet ((as-octet-array (contents)
           (make-array (length contents) :element-type '(unsigned-byte 8) :initial-contents contents)))
    (list (as-octet-array (list #xaa #xd3 #xdf #xcf #xec #xbc #xf5))
          (as-octet-array (list #xf5 #xe9 #xc6 #xe9 #xce #xa3 #xc1))
          (as-octet-array (list #xf2 #xef #xe9 #x11 #x0a #x2f #x9b))
          (as-octet-array (list #xdd #xec #xec #x79 #x5c #x89 #x0a))))
  "Octet arrays that, when spliced with other integers by #'empty-tile, result in empty PNG tile contents.
   The values are derived from real empty tiles. The empty tiles were identified with the help of zimcheck's output of duplicates.
   The first three octets represent the background color.
   See #'zim-contents for how the empty tiles get named.")

(defun empty-tile (identifying-part)
  "An octet array representing an empty PNG map tile with a certain IDENTIFYING-PART (see *identifying-parts-of-empty-tiles*).
   Empirical data shows that the empty tiles only differ by seven octets at positions from 41 below 48."
  (make-array
   +size-of-empty-tiles+
   :element-type '(unsigned-byte 8)
   :initial-contents
   (append (list 137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 4 0 0 0 4 0 1 3 0 0 0 69 211 185 192 0 0 0 3 80 76 84 69)
           (loop for byte across identifying-part collect byte)
           (list 0 0 0 150 73 68 65 84 120 156 237 193 1 1 0 0 0 130 32 255 175 110 72 64 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 239 6 4 30 0 1 159 33 210 230 0 0 0 0 73 69 78 68 174 66 96 130))))
(empty-tile (first *identifying-parts-of-empty-tiles*))

(defun empty-tile-target (empty-tile-candidate)
  "Whether the octet array EMPTY-TILE-CANDIDATE is - with a high probability - a known empty tile (according to *identifying-parts-of-empty-tiles*).
   If so, returns the ZIM-file-valid target URL for a redirect entry.
    The naming convention is synchronized with #'zim-contents.
   
   'High probability': tiles with +size-of-empty-tiles+ octets and matching bytes at positions from 41 below 48 perhaps really aren't empty tiles. But the risk of being wrong is taken (à la: feedback will tell)."
  (loop for i = 0 then (incf i)
        for part in *identifying-parts-of-empty-tiles*
        ;;#'equalp: surprisingly, #'equalp on sequences of 7 octets is quite fast (i.e. 1s for a maxzoom14-flavour full of sea tiles - completely acceptable).
        when (equalp part (subseq empty-tile-candidate 41 48))
          return (format nil "I/e~A.png" i)))
(empty-tile-target (empty-tile (second *identifying-parts-of-empty-tiles*)))


;;;; Metatile-related
;;;;  Metatiles are free locality. As a consequence, the tile reads get pooled by operating on the metatile level as long as possible.

#|
Information source: libapache2-mod-tile/src/metatile.cpp libapache2-mod-tile/includes/metatile.h.
Structure:
 tile-location:
  offset: (unsigned-byte 32)
   Based on 0 = start of file.
  size: (unsigned-byte 32)
 
 metatile-file:
  magic: (unsigned-byte 32)
   1096041805 "META"
   "METZ" for compressed, not implemented in libapache2-mod-tile.
  count: (unsigned-byte 32)
   Is (expt tiles-on-metatile-side 2).
  x, lowest x of metatile: (unsigned-byte 32)
  y, lowest y of metatile: (unsigned-byte 32)
  zoom: (unsigned-byte 32)
  Array of COUNT entries of tile-location.
  Body of COUNT appended tile contents (PNG).

The position of tile n is always defined at file-position (+ 20 (* 8 n)), and its length is available 4 bytes later.
 64 entries can be expected because the number of tiles on a metatile side (8) is defined in libapache2-mod-tile/includes/render_config.h, without the possibility for users to change this value without recompilation (not parsed in daemon.c).
  render_config.h says that the value should be a power of 2.
   → The real variable without functional dependency is +bits-for-metatile+ and thus 3.
   → Conversion to tiles-on-metatile-side is achieved by (ash 1 +bits-for-metatile+). The reverse is (integer-length tiles-on-metatile-side).
   → Conversion to COUNT is achieved by (ash 1 (ash +bits-for-metatile+ 1)). The reverse is (integer-length (integer-length COUNT)).
The origin of a tile is determined by the origin of its metatile and by the position of the tile in the metatile file. The origin of the metatile is defined at positions 8 for x and 12 for y.
 The origin of a metatile is not given by its directory hash, since the values gets masked in libapache2-mod-tile/src/store_file_utils.c.
|#

(defconstant +bits-for-metatile+ 3
  "Defines the loss of precision that is leveraged for metatile clustering.")

(defclass point-meta (point-renderd) ()
  (:documentation
   "renderd defines metatiles which contain tiles that share a common prefix controlled by +bits-for-metatile+.
    CACHE CONTROL: the coordinates are not updated should the undefined case of an update of a constant occur."))
(make-instance 'point-meta :x 123 :y 456 :z 3)

;;Adapted from libapache2-mod-tile/src/store_file_utils.c.
(defun metatile-value (number &optional (bits-for-metatile +bits-for-metatile+))
  "Set the last BITS-FOR-METATILE bits of NUMBER to 0."
  (deposit-field 0 (byte bits-for-metatile 0) number))

(defmethod initialize-instance :after ((object point-meta) &key)
  "Lose +bits-for-metatile+ digits of precision."
  (setf (x object) (metatile-value (x object))
        (y object) (metatile-value (y object))))

(defmethod (setf x) :around (new-x (instance point-meta))
  "Lose +bits-for-metatile+ digits of precision."
  (call-next-method (metatile-value new-x) instance))

(defmethod (setf y) :around (new-y (instance point-meta))
  "Lose +bits-for-metatile+ digits of precision."
  (call-next-method (metatile-value new-y) instance))

(defmethod reproject ((from point-renderd) (to (eql :point-meta)) &key)
  "Convert FROM (`point-renderd) to TO (`point-meta), which implies losing +bits-for-metatile+ precision."
  (make-instance 'point-meta :x (x from) :y (y from) :z (z from)))

(defclass mbr-meta (mbr) ()
  (:documentation "A MBR with LL UR being instances of `point-meta."))

(defmethod reproject ((from mbr-meta) (to (eql :mbr-meta)) &key) from)

(defmethod reproject ((from mbr-renderd) (to (eql :mbr-meta)) &key)
  "Reproject FROM to a `mbr-meta.
   ZOOM in interval [0..20]."
  (let* ((ll (reproject (ll from) :point-meta))
         (ur (reproject (ur from) :point-meta)))
    (make-instance 'mbr-meta
                   ;;Reprojection to `mbr-renderd already flipped the axis back.
                   :ll (make-instance 'point-meta :x (x ll) :y (y ll) :z (z (ll from)))
                   :ur (make-instance 'point-meta :x (x ur) :y (y ur) :z (z (ll from))))))

;;Adapted from libapache2-mod-tile/src/store_file_utils.c.
(defmethod metatile-dir-code ((point point-meta))
  "The last five numbers in the path of a metatile originated at POINT.
   The location code stores 40 interleaved bits in groups of 4+4 bits."
  (flet ((bits-of-group (number bitgroup)
           "The four bits in a NUMBER that belong to a BITGROUP, groups being LSB numbered."
           (ldb (byte 4 (* 4 bitgroup)) number)))
    (loop for bitgroup from 4 downto 0
          collect (logior (ash (bits-of-group (x point) bitgroup) 4)
                          (bits-of-group (y point) bitgroup)))))
;;Target: 33 6 80 247 128
(metatile-dir-code (make-instance 'point-meta :x 132600 :y 90224 :z 18))

;;Adapted from libapache2-mod-tile/src/store_file_utils.c (reversed).
(defgeneric meta-offset2renderd-offset (axis meta-offset)
  (:documentation "The metatile-local offset in the renderd coordinate system for the tile located at META-OFFSET within the metatile.")
  (:method ((axis (eql :x)) meta-offset)
    (ldb (byte +bits-for-metatile+ +bits-for-metatile+) meta-offset))
  (:method ((axis (eql :y)) meta-offset)
    (ldb (byte +bits-for-metatile+ 0) meta-offset)))
(meta-offset2renderd-offset :y 63)

(defmethod entries-in-a-metatile ((intersecting-point point-renderd) &key metatile-dir-base filtering-geometry)
  "List of (or dir-entry-tile dir-entry-redirect) with a valid SIZE value directly sourced from the metatile file.
   Lists all tiles in the metatile if FILTERING-GEOMETRY is nil.
   ;;Only lists tiles intersecting FILTERING-GEOMETRY (an instance of `mbr-3857) if CONSIDER-GEOMETRY? is non-nil."
  (declare (ignorable filtering-geometry))
  (let ((path (format nil "~A/~A/~{~A~^/~}.meta"
                      metatile-dir-base (z intersecting-point)
                      (metatile-dir-code (reproject intersecting-point :point-meta)))))
    (when (probe-file path)
      (with-open-file (f path
                         :element-type '(unsigned-byte 8))
        (file-position f 4)
        (let ((number-of-tiles (lisp-binary:read-integer 4 f))
              (metatile-origin-x (lisp-binary:read-integer 4 f))
              (metatile-origin-y (lisp-binary:read-integer 4 f)))
          (file-position f 20)
          (remove nil
                  (loop for i = 0 then (incf i)
                        for tile-offset-and-pos in (loop for i from 0 below number-of-tiles
                                                         ;;car=offset cadr=size
                                                         collect (list (lisp-binary:read-integer 4 f)
                                                                       (lisp-binary:read-integer 4 f)))
                        collect (let ((tile-x (+ metatile-origin-x (meta-offset2renderd-offset :x i)))
                                      (tile-y (+ metatile-origin-y (meta-offset2renderd-offset :y i))))
                                  ;;TODO: FILTERING-GEOMETRY.
                                  (when (and (plusp (cadr tile-offset-and-pos)))
                                    (let ((empty-tile-target
                                            (and (= +size-of-empty-tiles+ (cadr tile-offset-and-pos))
                                                 (empty-tile-target (progn (file-position f (car tile-offset-and-pos))
                                                                           (lisp-binary:read-bytes (cadr tile-offset-and-pos) f)))))
                                          (url (format nil "t/~A/~A/~A.png"
                                                       (z intersecting-point)
                                                       tile-x
                                                       tile-y)))
                                      (if empty-tile-target
                                          (make-instance 'dir-entry-redirect
                                                         :namespace "I" :url url
                                                         :target-url empty-tile-target)
                                          (make-instance 'dir-entry-tile
                                                         :namespace "I" :url url
                                                         :mimetype-id :png
                                                         :absolute-path-of-its-metatile path
                                                         :offset (car tile-offset-and-pos)
                                                         :size (cadr tile-offset-and-pos)))))))))))))
;;(entries-in-a-metatile (make-instance 'point-meta :x 1 :y 2 :z 3) :metatile-dir-base "/mnt/main/mod_tile/osm1024/")

(defun dir-entry-tiles (&key flavour metatile-dir-base)
  "Gather all tiles intersecting (geometry FLAVOUR)."
  (let ((zoom-delta 2)
        (tiles-on-metatile-side (expt 2 +bits-for-metatile+)))
    (loop for zoom
          from (as-non-256px-zoom (zoom-min flavour) (tile-size flavour))
          upto (as-non-256px-zoom (zoom-max flavour) (tile-size flavour))
          by zoom-delta
          append (let ((my-mbr (reproject (geometry flavour) :mbr-renderd :zoom zoom :filter-on-low-zoom-levels-too? nil)))
                   (loop for x from (x (ll my-mbr)) upto (+ (x (ur my-mbr)) #|tiles-on-metatile-side|#) by tiles-on-metatile-side
                         append (loop for y from (y (ll my-mbr)) upto (+ (y (ur my-mbr)) #|tiles-on-metatile-side|#) by tiles-on-metatile-side
                                      append (entries-in-a-metatile
                                              (make-instance 'point-renderd :x x :y y :z zoom)
                                              :metatile-dir-base metatile-dir-base)))))))
;;(dir-entry-tiles :flavour *selected-flavour* :metatile-dir-base "/mnt/main/mod_tile/osm1024/")

