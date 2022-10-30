;;;; TOO (The Offline Oriented) creates ZIM files with offline available maps
;;;; Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>
;;;; 
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :too)

;;;; Defining and filling the structure needed to represent a release or flavours of a release.
;;;; Also see docs/glossary.txt.

;;;; Layers see layer.lisp.

;;;; OpenStreetMap Wiki definitions see osmwiki-definition.lisp.

;;;; Mapnik zoom levels.

(defparameter *zoom-levels*
  ;;zoom level number → `zoom-level instance
  (make-hash-table))

(defclass zoom-level ()
  ((level :accessor level :initarg :level :initform nil)
   (nickname :accessor nickname :initarg :nickname :initform nil)
   (description :accessor description :initarg :description :initform nil))
  (:documentation "Information functionally dependent on the zoom level number."))

(defun zoom-nickname (level)
  "The nickname of a zoom LEVEL according to *zoom-levels*."
  (nickname (gethash level *zoom-levels*)))


;;;; Regions used to produce ZIM files.

(defparameter *regions*
  ;;nickname (string) → `region instance
  (make-hash-table :test #'equal))

(defclass region ()
  ((nickname :accessor nickname :initarg :nickname :initform nil :documentation "\"prototype\" won't fill TOO.zip with meaningful contents.")
   (user-facing-name :accessor user-facing-name :initarg :user-facing-name :initform nil :documentation "Is used within sentences. The first letter automatically gets upcased for the ZIM file description.")
   ;;TODO: Multiple MBRs as the next step, multipolygons with holes etc. afterwards.
   (geometry :accessor geometry :initarg :geometry :initform nil :documentation "Currently an instance of `mbr-4326. Gets #'clamped to *3857-aware-earth*."))
  (:documentation "The TOO project may render to different regions."))

(defmethod (setf geometry) :around (new-geometry (instance region))
  "Ensure that the GEOMETRY of a region INSTANCE is clamped to *3857-aware-earth*."
  (call-next-method (clamp-mbr new-geometry) instance))
(setf (geometry (make-instance 'region :nickname "abc"))
      (make-instance 'mbr-4326 :ll-lat -90 :ll-lon -180 :ur-lat 90 :ur-lon 180))

(defmethod initialize-instance :after ((instance region) &key)
  "Ensure that the GEOMETRY of a region INSTANCE is clamped to *3857-aware-earth*."
  (when (geometry instance)
    (setf (geometry instance) (clamp-mbr (geometry instance)))))


;;;; Planet file (a.k.a. ground truth)

(defclass planet-file ()
  ((filesize :accessor filesize :initarg :filesize :initform nil :documentation "Size in bytes. Use #'gib helper for user-facing value.")
   (year :accessor year :initarg :year :initform nil)
   (month :accessor month :initarg :month :initform nil)
   (day :accessor day :initarg :day :initform nil)
   (nodes :accessor nodes :initarg :nodes :initform nil :documentation "Number of nodes in a planet file.")
   (ways :accessor ways :initarg :ways :initform nil :documentation "Number of ways in a planet file.")
   (relations :accessor relations :initarg :relations :initform nil :documentation "Number of relations in a planet file."))
  (:documentation
   "Metadata surrounding a planet file. Time informations describe ground truth state, not necessarily the planet file publication date.
    Even regional extracts finally refer to a planet file. The slots of this class really should describe the planet file any data is based on."))

(defun gib (bytes)
  "Human readable BYTES shown as string containing one decimal place, with GiB suffix."
  (format nil "~,1F GiB" (/ bytes (expt 2 30))))
(gib 69099243792)


;;;; Release

(defclass release
    (
     ;;Ground truth of the tiles and layers.
     planet-file
     ;;A geometric representation of the area for which tiles and layers are included. Very low zoom levels may be hardcoded to include the whole world since this will result in a zooming experience into the target region without missing tiles. Not that the release is set to initially show the world instead of the region.
     region)
  ((layer-references :initarg :layer-references :initform nil :documentation "List of strings being the key of a layer in *layers*.")
   (tile-size :accessor tile-size :initarg :tile-size :initform 1024 :documentation "Number of pixels on a tile side. Depends on a release since the flavours may wish for impossible zoom levels. CACHE CONTROL: update the values for empty map tiles in src/map-tiles.lisp if you use other values than 1024.")
   (release-max-zooms :accessor release-max-zooms :initarg :release-max-zooms :initform nil :documentation "List of maximal zoom levels per flavour.")
   (own-nickname :accessor own-nickname :initarg :own-nickname :initform nil :documentation "If the topic is more important than the region chosen, or if the topic is to be prepended to the region name, use this slot to steer what will show up in the region place of the ZIM file name. If empty, the users can assume a default layer selection, but there are no guarantees regarding layer selection expectations. If possible, do not use this slot because of the increase of entries in the list of available ZIM files (which all have to be created/published (regularly)).")))

(defmethod final-nickname ((object release))
  "The final nickname that should be used in #'zim-file-name."
  (or (own-nickname object) (nickname object)))

;;Reasoning for this approach: compiling layers.lisp alone results in bubbled-up information in src/user-interface.lisp this way.
(defmethod layers ((object release))
  "A list of layer instances referenced by a release.
   References *layers*.
   Not setfable."
  (mapcar (lambda (x)
            (gethash x *layers*))
          (slot-value object 'layer-references)))

(defun render-list-mbr-commands (&key (path "/dev/shm/temp") (splice-options '("--all")) release-or-flavour economically-use-ram?)
  "render_list commands suitable for bulk-rendering a RELEASE."
  (with-open-file (stream path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (loop for zoom in (mapcar (lambda (x) (as-non-256px-zoom x (tile-size release-or-flavour)))
                              (sort (remove-duplicates
                                     (loop for max-zoom in (release-max-zooms release-or-flavour)
                                           append (quick-flavour-zoom-levels max-zoom (tile-size release-or-flavour))))
                                    #'<))
          do (progn
               ;;renderd seems to accumulate cache.
               (when (and (< 0 zoom) economically-use-ram?)
                 ;;sudo -S
                 ;;while [ true ]; do sudo -u root -v; sleep 840; done
                 (format stream "sudo -u root systemctl restart renderd~%"))
               (let ((mbr (reproject (geometry release-or-flavour) :mbr-renderd :zoom zoom)))
                 (format stream (dsl " "
                                     "render_list"
                                     splice-options
                                     (format nil "--num-threads=~A"
                                             (if economically-use-ram?
                                                 (let ((256px-zoom (as-256px-zoom zoom (tile-size release-or-flavour))))
                                                   (cond ((<= 0 256px-zoom 8) 1)
                                                         ((<= 9 256px-zoom 10) 4)
                                                         ((<= 11 256px-zoom 20) 12)
                                                         (T 1)))
                                                 8))
                                     "-z" zoom
                                     "-Z" zoom
                                     "-x" (x (ll mbr))
                                     "-X" (x (ur mbr))
                                     "-y" (y (ll mbr))
                                     "-Y" (y (ur mbr))
                                     #\Newline)))))))

(defclass flavour (release)
  ((zoom-levels :accessor zoom-levels :initarg :zoom-levels :initform nil :documentation "Sorted list of numbers which are the key for *zoom-levels*."))
  (:documentation "For each release, multiple flavours differing in maximally supported zoom levels (= file sizes) are chosen. This class represents such a flavour. Zoom levels are to be interpreted as 256px zoom levels."))

(defmethod initialize-instance :after ((instance flavour) &key planet region)
  "Convenient #'make-instance for a `flavour: just pass instances of flavour's superclasses.
   PLANET: instance of `planet-file.
   REGION: instance of `region."
  (flet ((supersplice (splicing-instance)
           "Splice SPLICING-INSTANCE into INSTANCE.
            The class of SPLICING-INSTANCE must be a (possibly indirect) superclass of INSTANCE."
           (loop for slot in (closer-mop:class-slots (class-of splicing-instance))
                 do (setf (slot-value instance (closer-mop:slot-definition-name slot))
                          (slot-value splicing-instance (closer-mop:slot-definition-name slot))))))
    (supersplice planet)
    (supersplice region)))

(defmethod zoom-min ((object flavour))
  "Minimally supported zoom level (key for *zoom-levels*) of a flavour."
  (car (zoom-levels object)))

(defmethod zoom-max ((object flavour))
  "Maximally supported zoom level (key for *zoom-levels*) of a flavour."
  (car (last (zoom-levels object))))

(defun quick-flavour-zoom-levels (zoom-max tile-size)
  "256px-zoom levels which are included in a flavour. Assumes a zoom delta of 2."
  (loop for zoom
        ;;Don't start with smaller 256px-zoom-levels than are possible with TILE-SIZE.
        from (+ (- (zoom-offset tile-size))
                (if (evenp zoom-max) 0 1))
        ;;By 2: TOO uses delta 2 since there is limited space for/in ZIM files.
        upto zoom-max by 2
        collect zoom))
(loop for zoom from 0 upto 20 collect (quick-flavour-zoom-levels zoom 1024))

(defparameter *flavours* (list)
  "Chronological list of flavours, sorted by descending release date, then ascending (zoom-max flavour-instance).")

(defparameter *default-layers*
  (list "hospitals" "no_emergency_medicare" "pharmacies" "chemists" "police_stations" "libraries" "public_transport_stops" "fuel" "drinking_water" "toilets" "shower" "laundries_lavoirs" "shelter" "social_facility")
  "The typical layers that are used by default.")

(defun register-release-flavours (&key planet region (layer-references *default-layers*) (tile-size 1024) own-nickname zoom-mini zoom-maxi)
  "Construct multiple flavours (i.e. construct a full release with all the details) and push them to *flavours*."
  (loop for flavour in (reverse
                        (loop for zoom-max from zoom-mini upto zoom-maxi
                              collect (make-instance
                                       'flavour
                                       :planet planet
                                       :region region
                                       :layer-references layer-references
                                       :tile-size tile-size
                                       :own-nickname own-nickname
                                       :release-max-zooms (loop for zoom from zoom-mini upto zoom-maxi collect zoom)
                                       :zoom-levels (quick-flavour-zoom-levels zoom-max tile-size))))
        do (push flavour *flavours*)))


;;;; Data

(loop for zoom-level in (list (list :level 9 :nickname "regionoverview" :description "An overview map showing the cities in context.")
                              (list :level 10 :nickname "cityoutlines" :description "The outlines of cities are clearly visible.")
                              (list :level 11 :nickname "bigroads" :description "Shows large or important roads within the city.")
                              (list :level 12 :nickname "cityoverview" :description "Overview of a city. Potentially usable for navigation.")
                              (list :level 13 :nickname "citymaps" :description "Zoom level 13 clearly maps the city layout. Dense small street networks, although somehow drawn, may be usable or not.")
                              (list :level 14 :nickname "buildingoutlines" :description "Most of the times buildings are discernable.")
                              (list :level 15 :nickname "usablefootpaths" :description "Footpaths are usable for navigation.")
                              (list :level 16 :nickname "firstpois" :description "The first points of interest (POIs) are symbolized.")
                              (list :level 17 :nickname "housenumbers" :description "House numbers of most houses become visible.")
                              (list :level 18 :nickname "poipoipoi" :description "Most POIs show up. They are only labeled if there is enough map space left.")
                              (list :level 19 :nickname "labeledpois" :description "Most POIs are labeled.")
                              (list :level 20 :nickname "immersed" :description "This zoom level provides the greatest detail available."))
      do (setf (gethash (getf zoom-level :level) *zoom-levels*)
               (make-instance 'zoom-level
                              :level (getf zoom-level :level)
                              :nickname (getf zoom-level :nickname)
                              :description (getf zoom-level :description))))

(loop for region in (list
                     ;;Earth is an instance of a planet, just like other celestial bodies such as Mars are, too.
                     (list :nickname "earth"
                           :user-facing-name "the whole Earth"
                           :geometry (make-instance 'mbr-4326 :ll-lat -90
                                                              :ll-lon -180
                                                              :ur-lat 90
                                                              :ur-lon 180))
                     (list :nickname "prototype"
                           :user-facing-name "a part of London (the region for the prototype)"
                           :geometry (make-instance 'mbr-4326 :ll-lat 51.5 :ll-lon -0.4 :ur-lat 51.7 :ur-lon 0.4))
                     (list :nickname "nyc"
                           :user-facing-name "New York"
                           :geometry (make-instance 'mbr-4326 :ll-lat 40.4754 :ll-lon -74.256 :ur-lat 40.9618 :ur-lon -73.7012)))
      do (setf (gethash (getf region :nickname) *regions*)
               (make-instance 'region
                              :nickname (getf region :nickname)
                              :user-facing-name (getf region :user-facing-name)
                              :geometry (getf region :geometry))))

(let ((planet (make-instance 'planet-file
                             :filesize 70417833793
                             :year 2022 :month 10 :day 3
                             :nodes 7950120815 :ways 891190805 :relations 10285396)))
  (register-release-flavours
   :planet planet
   :region (gethash "earth" *regions*)
   :zoom-mini 9 :zoom-maxi 14)
  (register-release-flavours
   :planet planet
   :region (gethash "prototype" *regions*)
   :tile-size 256
   :zoom-mini 9 :zoom-maxi 14)
  (register-release-flavours
   :planet planet
   :region (gethash "nyc" *regions*)
   :tile-size 256
   :zoom-mini 9 :zoom-maxi 14))

