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
An implementation of the ZIM file format that, unlike libzim, is focussed on writing out ZIM files (for TOO).
 The idea of placing the clusters in front of the pointer lists is from libzim/src/writer/creator.cpp.
 The format is described and exemplified in the openZIM wiki.

Motivation: large flavours, when created with zimwriterfs, did't fit into /dev/shm anymore, even when transparently compressed. And caching the unpackaged contents deeper in the storage hierarchy is s...l...o...w. A solution that is able to discard map tiles once written is needed.
 Unsure about the caching behaviour of lisp-binary. Caching all clusters is not an option.
|#

;;; General

(defparameter *flexi-utf8* (flexi-streams:make-external-format :utf-8))

(defparameter *empty-favicon*
  (let ((png '(137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 48 0 0 0 48 8 0 0 0 0 114 105 166 91 0 0 0 32 73 68 65 84 72 199 99 248 79 34 96 24 213 48 170 97 84 195 168 134 81 13 163 26 70 53 140 106 24 89 26 0 27 15 247 121 105 20 20 206 0 0 0 0 73 69 78 68 174 66 96 130)))
    (make-array (length png) :element-type '(unsigned-byte 8) :initial-contents png))
  "A white 48px*48px square in PNG, pngcrushed with --brute.
   Intended for development where zimcheck is leveraged as external compliance checker. zimcheck FAILs the whole file when the favicon is missing.")


;;; MIME list

(defparameter *mimes-of-too* '("application/javascript"
                               ;;Opinion: there is no need for a fulltext index in TOO. Place names are searchable, and layer data is searchable with JavaScript ("Name lookup") or used in a spatial query setting. The few pages are maximally two steps away from the welcome file.
                               ;;"application/octet-stream+xapian"
                               "application/pdf"
                               "application/x-blender"
                               "application/xml"
                               "application/zip"
                               "image/png"
                               "image/svg+xml"
                               "text/css"
                               "text/html"
                               "text/plain")
  "Materialized list of MIMEs seen in TOO.")

(defgeneric mimeq (my-mime)
  (:documentation "Returns the position of the expanded MIME type in *mimes-of-too*.")
  (:method ((my-mime string))
    (or (position my-mime *mimes-of-too* :test #'string=)
        (error "#'mimeq is not synchronized to *mimes-of-too*.")))
  (:method ((my-mime (eql :js)))
    (mimeq "application/javascript"))
  (:method ((my-mime (eql :pdf)))
    (mimeq "application/pdf"))
  (:method ((my-mime (eql :xml)))
    (mimeq "application/xml"))
  (:method ((my-mime (eql :zip)))
    (mimeq "application/zip"))
  (:method ((my-mime (eql :png)))
    (mimeq "image/png"))
  (:method ((my-mime (eql :svg)))
    (mimeq "image/svg+xml"))
  (:method ((my-mime (eql :css)))
    (mimeq "text/css"))
  (:method ((my-mime (eql :html)))
    (mimeq "text/html"))
  (:method ((my-mime (eql :txt)))
    (mimeq "text/plain"))
  (:method ((my-mime (eql :blend)))
    (mimeq "application/x-blender")))
(mimeq :html)

(defun mime-type-list (&optional (mimes *mimes-of-too*))
  "Returns the MIME type list MIMES of a TOO ZIM file as an octet array."
  (flexi-streams:with-output-to-sequence (stream)
    (loop for mime in mimes
          do (write-sequence (flexi-streams:string-to-octets mime :external-format *flexi-utf8*) stream)
          do (write-byte 0 stream))
    (write-byte 0 stream)))
(mime-type-list)


;;; Directories and directory entries

(defclass dir-entry ()
  ((namespace :accessor namespace :initarg :namespace :initform "A" :documentation "String with one character.")
   (url :accessor url :initarg :url :initform nil :documentation "String. The part following the namespace character plush slash in its final state, relative to the `dir instance this instance belongs to otherwise.")
   (title :accessor title :initarg :title :initform nil :documentation "String. The ZIM format specifies that the title is determined according to the rule (or TITLE (format nil \"~A~A\" NAMESPACE URL))."))
  (:documentation
   "The directory entry a.k.a. \"dirent\".
    The interesting part is the URL with its flat approach towards nesting. As directories are useful for isolation indeed, instances of `dir-entry will be gathered in instances of `dir and their effective URL will get built by #'flatten-dir."))

(defclass dir-entry-redirect (dir-entry)
  ((target-url :accessor target-url :initarg :target-url :initform nil :documentation "#'resolve tests this redirect target with #'string= against the namespace-prefixed final #'url of other directory entries. Which means that this slot is a namespace-prefixed `dir-dissolved URL path (see #'flatten-dir).")
   (resolved-position :accessor resolved-position :initarg :resolved-position :initform nil :documentation "Position in a list of sorted directory entries. For late stages shortly before render time. Has precedence over TARGET-URL if non-nil."))
  (:documentation "'Redirect' directory entries point to other existing content directory entries."))

(defclass dir-entry-content (dir-entry)
  ((mimetype-id :accessor mimetype-id :initarg :mimetype-id :initform nil :documentation "Integer, but you can conveniently use the keyword specializers of #'mimeq for this slot, too. ZIM file 1:n mimetype-id.")
   (cluster-id :accessor cluster-id :initarg :cluster-id :initform nil :documentation "ZIM file 1:n cluster-id.")
   (blob-id :accessor blob-id :initarg :blob-id :initform nil :documentation "cluster-id 1:n blob-id."))
  (:documentation "A directory entry which will have associated data in the clusters."))

(defmethod (setf mimetype-id) :around (new-mimetype-id (instance dir-entry-content))
  "Conveniently use the keyword specializers of #'mimeq for MIMETYPE-ID."
  (call-next-method (if (keywordp new-mimetype-id)
                        (mimeq new-mimetype-id)
                        new-mimetype-id)
                    instance))
(setf (mimetype-id (make-instance 'dir-entry-content :title "abc")) :txt)

(defmethod initialize-instance :after ((instance dir-entry-content) &key)
  "Conveniently use the keyword specializers of #'mimeq for MIMETYPE-ID."
  (when (keywordp (mimetype-id instance))
    (setf (mimetype-id instance) (mimetype-id instance))))
(make-instance 'dir-entry-content :title "abc" :mimetype-id :txt)

(defclass dir-entry-file (dir-entry-content)
  ((prepend-path :accessor prepend-path :initarg :prepend-path
                 :initform (cl-fad:merge-pathnames-as-directory
                            (user-homedir-pathname)
                            "quicklisp/local-projects/too/skel/")
                 :allocation :class :documentation "Some sort of working directory for the assets that are gathered in TOO.")
   (relative-path
    :accessor relative-path :initarg :relative-path :initform nil
    :documentation "A path to a file, relative to PREPEND-PATH.
                    TRICK: as this value is passed as the last argument to cl-fad:merge-pathnames-as-file, you can use an absolute path in this slot if you want so."))
  (:documentation "A `dir-entry-content with contents sourced from a file."))

(defmethod absolute-path ((entry dir-entry-file))
  "The absolute path of a `dir-entry-file.
   Does not contact the file system."
  (when (relative-path entry)
    (cl-fad:merge-pathnames-as-file (prepend-path entry)
                                    (relative-path entry))))

(defclass dir-entry-memory (dir-entry-content)
  ((content :accessor content :initarg :content :initform nil :documentation "Supported type: (or string (simple-array (unsigned-byte 8) (*)))."))
  (:documentation "A `dir-entry-content that stores its contents in memory."))

(defclass dir-entry-tile (dir-entry-content)
  ((absolute-path-of-its-metatile :accessor absolute-path-of-its-metatile :initarg :absolute-path-of-its-metatile :initform nil :documentation "The absolute path to the metatile the tile is stored in.")
   (offset :accessor offset :initarg :offset :initform nil :documentation "The offset within the metatile from which the tile starts.")
   (size :accessor size :initarg :size :initform nil :documentation "Caches the size of the tile. This is needed since tiles don't populate CONTENT nor REAL-PATH."))
  (:documentation "Tiles are added to the ZIM file in a special fashion. See #'dir-entry-tiles."))

;;TODO: automatically synchronize initforms.
(defun dir-entry (&key (namespace "A") url target-url title mimetype-id content relative-path cluster-id blob-id)
  "Constructor for redirect entries or non-tile dir entries with contents.
   Conveniently instantiates the matching subclass.
   Precedence order: TARGET-URL RELATIVE-PATH CONTENT."
  (cond (target-url (make-instance 'dir-entry-redirect
                                   :namespace namespace :url url :title title :target-url target-url))
        (relative-path (make-instance 'dir-entry-file
                                      :namespace namespace :url url :title title :mimetype-id mimetype-id
                                      :cluster-id cluster-id :blob-id blob-id
                                      :relative-path relative-path))
        (content (make-instance 'dir-entry-memory
                                :namespace namespace :url url :title title :mimetype-id mimetype-id
                                :cluster-id cluster-id :blob-id blob-id
                                :content content))
        (T (error "Quality control: there is no reason why a dir-entry without a finally determinable length should get defined."))))

(defun metaq (key value &optional (my-mime :txt) relative-path)
  "Quick helper which produces a metadata `dir-entry."
  (dir-entry :namespace "M" :url key :title key :content value
             :relative-path relative-path :mimetype-id (mimeq my-mime)))

(defgeneric length-coarse (entry)
  (:method ((entry dir-entry-redirect))
    "Redirects don't contribute to clusters."
    0)
  (:method ((entry dir-entry-tile))
    "Tile size."
    (size entry))
  (:method ((entry dir-entry-file))
    "File size."
    (let ((absolute-path (absolute-path entry)))
      (when (and absolute-path (probe-file absolute-path))
        (trivial-file-size:file-size-in-octets absolute-path))))
  (:method ((entry dir-entry-memory))
    "The length of the character string in (content DIR-ENTRY).
     The length is only coarse since this method has no concept of external formats."
    (length (content entry)))
  (:documentation "Possibly rough estimation of the cluster contribution of a `dir-entry in octets."))

(defclass dir ()
  ((name :accessor name :initarg :name :initform nil :documentation "String.")
   (entries :accessor entries :initarg :entries :initform nil :documentation "List of (or dir-entry dir)."))
  (:documentation "Logical directory that will get materialized in the URLs of its leaf `dir-entry instances."))

(defun dir (&key name entries)
  "Constructor for `dir."
  (make-instance 'dir :name name :entries entries))

(defparameter *zim-example*
  (labels ((html-page (title content)
             (x-html (x-head (x-title title)
                             "<meta charset=\"utf-8\">")
                     (x-body content)))
           (entryq (url title content)
             (dir-entry :url url :title title :content (html-page title content) :mimetype-id (mimeq :html) :namespace "A")))
    (dir
     :name "too"
     :entries (list (metaq "Name" "vehicle_types_wiki")
                    (metaq "Title" "Vehicle Types Wiki")
                    (metaq "Creator" "Benedikt Steger")
                    (metaq "Publisher" "Benedikt Steger")
                    (metaq "Date" (let ((now (multiple-value-list (get-decoded-time))))
                                    (format nil "~A-~2,'0D-~2,'0D" (sixth now) (fifth now) (fourth now))))
                    (metaq "Description" "A small example wiki that starts a vehicle categorization project.")
                    (metaq "LongDescription" "Long description paragraph.")
                    (metaq "Language" "eng")
                    (metaq "License" "AGPLv3+")
                    (metaq "Tags" "_pictures:no;_videos:no;_details:yes;_ftindex:no")
                    (metaq "Flavour" "prototype")
                    (metaq "Scraper" "TOO")
                    (metaq "Illustration_48x48@1" *empty-favicon* :png)
                    (dir-entry :namespace "-" :url "favicon" :target-url "M/Illustration_48x48@1")
                    (dir-entry :namespace "W" :url "mainPage" :target-url "A/index.html")
                    (dir-entry :namespace "I" :url "background_cropped_bright_navigable.png" :content *empty-favicon* :relative-path "background_cropped_bright_navigable.png" :mimetype-id :png)
                    (entryq "index.html" "Vehicle types"
                            (dsl ""
                                 "<h1>Vehicle types</h1><p>There are many vehicle types. They can be roughly categorized into <b>terrain-based</b> "
                                 (dsl '("(" ", " ", etc.)")
                                      "<a href='terrain-based/Automobile.html'>automobiles</a>"
                                      "<a href='terrain-based/Caterpillar.html'>caterpillars</a>")
                                 ", <b>amphibian</b> (<a href='amphibian/Boat.html'>boats</a> such as <a href='amphibian/specific-boats/Dinghy.html'>dinghies</a>) and <b>airborne</b>. The presented categorization, while sufficient for fleshing out edge cases during development, is only exemplary of course.</p><p>content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content content</p><p>Image: <img src='../I/background_cropped_bright_navigable.png'></p>"))
                    (dir
                     :name "terrain-based"
                     :entries (list (entryq "Auto.html" "Auto" "<h1>Auto</h1>An auto is a vehicle with four tires. Despite their name, most automobiles are not self-driving. Here is a symbol of a car: 🚘.")
                                    (make-instance 'dir-entry-redirect :url "Automobile.html" :target-url "A/terrain-based/Auto.html")
                                    (entryq "Caterpillar.html" "Caterpillar" "<h1>Caterpillar</h1>A caterpillar, although also sometimes equipped with four tires, is a vehicle designed for moving materials around, but, unlike lorries, ...")))
                    (dir
                     :name "amphibian"
                     :entries (list (entryq "Boat.html" "Boat" "<h1>Boat</h1>A boat is an amphibian ...")
                                    (dir
                                     :name "specific-boats"
                                     :entries (list (entryq "Dinghy.html" "Dinghy" "<h1>Dinghy</h1>A dinghy is a <a href=\"../Boat.html\">boat</a> that...")))))
                    (dir :name "airborne")))))

(defun flatten-dir (dir &optional (path ()))
  "Flatten DIR and produce correct URLs in the leaf `dir-entry instances.
    Correct := relative to DIR.
   PATH is internal.
   Also see #'flatten."
  (if (typep dir 'dir)
      (loop for entry in (entries dir)
            append (let ((new-path (cons (name dir) path)))
                     (if (typep entry 'dir)
                         (flatten-dir entry new-path)
                         (list (dir-entry
                                :namespace (namespace entry)
                                :url (concatenate 'string
                                                  (dsl "/" (cdr (reverse new-path)))
                                                  (when path "/")
                                                  (url entry))
                                :target-url (when (typep entry 'dir-entry-redirect)
                                              (target-url entry))
                                :title (title entry)
                                :mimetype-id (when (typep entry 'dir-entry-content)
                                               (mimetype-id entry))
                                :content (when (typep entry 'dir-entry-memory)
                                           (content entry))
                                :relative-path (when (typep entry 'dir-entry-file)
                                                 (relative-path entry))
                                :cluster-id (when (typep entry 'dir-entry-content)
                                              (cluster-id entry))
                                :blob-id (when (typep entry 'dir-entry-content)
                                              (blob-id entry)))))))
      dir))
(flatten-dir *zim-example*)

(defun prepend-mime-histogram (dir-entries)
  "Counts the number of MIME occurencence in DIR-ENTRIES, a list of `dir-entry instances.
   Returns (cons M/Counter DIR-ENRIES).
   Since the definition of M/Counter is 'Number of non-redirect entries per mime-type', M/Counter is included in the histogram, too."
  ;;mimetype-id → count
  (let ((count-group-by-mime (make-hash-table)))
    (loop for entry in dir-entries
          when (typep entry 'dir-entry-content)
            do (incf (gethash (mimetype-id entry) count-group-by-mime 0)))
    (incf (gethash (position "text/plain" *mimes-of-too* :test #'string=) count-group-by-mime 0))
    (cons (dir-entry :namespace "M"
                     :url "Counter"
                     :title "Counter"
                     :mimetype-id :txt
                     :content (dsl ";"
                                   (loop for k being the hash-key of count-group-by-mime
                                         for v being the hash-value of count-group-by-mime
                                         collect (dsl "=" (elt *mimes-of-too* k) (gethash k count-group-by-mime)))))
          dir-entries)))
(prepend-mime-histogram (flatten-dir *zim-example*))

;;2 MiB: value from zimwriterfs.
(defun enumerate-cluster-blobs! (&key remaining-dir-entries (target-cluster-size (* 2 1024 1024))
                                   (cluster-id 0) (blob-id 0)
                                   (accumulated-cluster-space 0) accumulated-dir-entries)
  "Assign CLUSTER-ID BLOB-ID to the first `dir-entry in REMAINING-DIR-ENTRIES, and increase them for the recursively called rest.
   TARGET-CLUSTER-SIZE governs when CLUSTER-ID has to increase, but note that there are no real guarantees. CLUSTER-ID is only increased after TARGET-CLUSTER-SIZE is overshot. Additionally, the length of the `dir-entry is determined with #'coarse-length, which has no concept of external formats.
   Destructively modifies REMAINING-DIR-ENTRIES.
   CLUSTER-ID BLOB-ID could theoretically be used to declare an offset, and ACCUMULATED-CLUSTER-SPACE could be helpful for a partially-filled cluster.
   But ACCUMULATED-DIR-ENTRIES is definitively internal."
  (if remaining-dir-entries
      (let* ((entry (car remaining-dir-entries))
             (length-coarse (length-coarse entry))
             (new-cluster? (< target-cluster-size
                              (+ accumulated-cluster-space length-coarse))))
        (when (typep entry 'dir-entry-content)
          (setf (cluster-id entry) cluster-id
                (blob-id entry) blob-id))
        (enumerate-cluster-blobs!
         :remaining-dir-entries (cdr remaining-dir-entries)
         :target-cluster-size target-cluster-size
         ;;Knowing that redirects contribute 0 to ACCUMULATED-CLUSTER-SPACE.
         :cluster-id (if new-cluster? (1+ cluster-id) cluster-id)
         :blob-id (cond ((and (typep entry 'dir-entry-content) new-cluster?) 0)
                        ((typep entry 'dir-entry-content) (1+ blob-id))
                        (T blob-id))
         :accumulated-dir-entries (cons entry accumulated-dir-entries)
         :accumulated-cluster-space (+ (if new-cluster? 0 accumulated-cluster-space)
                                       length-coarse)))
      (reverse accumulated-dir-entries)))
(loop for target-cluster-size in '(10 1000 100000000)
      collect (enumerate-cluster-blobs!
               :remaining-dir-entries (flatten-dir *zim-example*)
               :target-cluster-size target-cluster-size))

(defun resolve (namespace/url entries-order-by-url)
  "The position of the first entry with #'string= -matching (namespace entry) (url entry) in ENTRIES-ORDER-BY-URL."
  (position namespace/url
            entries-order-by-url
            :key (lambda (x) (concatenate 'string (namespace x) "/" (url x)))
            :test #'string=))
(resolve "M/Illustration_48x48@1" (flatten-dir *zim-example*))
(resolve "A/terrain-based/Auto.html" (flatten-dir *zim-example*))

(defun resolve-redirects! (entries-order-by-url)
  "Populates RESOLVED-POSITION of `dir-entry-redirect instances in ENTRIES-ORDER-BY-URL and returns the modified ENTRIES-ORDER-BY-URL.
   ENTRIES-ORDER-BY-URL really must be presorted according to the ZIM file specification since the resolved indices are meaningless otherwise.
   Destructively modifies ENTRIES-ORDER-BY-URL."
  ;;TARGET-URL → integer position in ENTRIES-ORDER-BY-URL
  (let ((lookup-cache (make-hash-table :test 'equal)))
    (loop for entry in entries-order-by-url
          collect (progn (when (typep entry 'dir-entry-redirect)
                           (setf (resolved-position entry)
                                 (or (gethash (target-url entry) lookup-cache)
                                     (setf (gethash (target-url entry) lookup-cache)
                                           (resolve (target-url entry) entries-order-by-url)))))
                         entry))))
#|(resolve-redirects! (sort (append (flatten-dir (zim-contents :flavour *selected-flavour*))
                                  (dir-entry-tiles :flavour *selected-flavour* :metatile-dir-base "/mnt/osmbase/mod_tile/osm1024/"))
                          #'string< :key (lambda (x) (concatenate 'string (namespace x) (url x)))))|#


;;; Directory entry output

(defun write-dir-entry (dir-entry stream)
  "Writes DIR-ENTRY, an instance of `dir-entry, to STREAM.
   The content is handled by #'write-cluster."
  (write-integer (if (typep dir-entry 'dir-entry-redirect)
                     (1- (ash 1 16))
                     (mimetype-id dir-entry))
                 2 stream)
  ;;Length of extra parameters.
  (write-byte 0 stream)
  (write-byte (char-int (aref (namespace dir-entry) 0)) stream)
  ;;Revision.
  (write-sequence #(0 0 0 0) stream)
  (cond ((typep dir-entry 'dir-entry-content)
         (write-integer (cluster-id dir-entry) 4 stream)
         (write-integer (blob-id dir-entry) 4 stream))
        ((typep dir-entry 'dir-entry-redirect)
         (write-integer (resolved-position dir-entry) 4 stream))
        (T (error "Either an instance of `dir-entry-content or `dir-entry-redirect is expected.")))
  (write-sequence (flexi-streams:string-to-octets (url dir-entry) :external-format *flexi-utf8*) stream)
  ;;Zero-terminated URL.
  (write-byte 0 stream)
  (write-sequence (flexi-streams:string-to-octets (title dir-entry) :external-format *flexi-utf8*) stream)
  ;;Zero-terminated TITLE.
  (write-byte 0 stream))
(flexi-streams:with-output-to-sequence (stream)
  (write-dir-entry (dir-entry
                    :url "Auto"
                    :title ""
                    :mimetype-id :html
                    :namespace "A"
                    :content "<h1>Auto</h1>"
                    :cluster-id 0
                    :blob-id 0)
                   stream))


;;; Cluster output

(defun offsets-plus-endpos (&key (remaining-lengths) (multiplier 4) (offset 0) (offset-list ()))
  "Offsets to starting positions of blobs (with lengths given by REMAINING-LENGTHS) when they are logically linearly concatenated into a single blob, and appended to this result, under the assumption that each result number occupies MULTIPLIER bytes.
   Includes the position after the last blob.
   OFFSET OFFSET-LIST are internal."
  (if remaining-lengths
      (offsets-plus-endpos :remaining-lengths (cdr remaining-lengths)
                           :offset (+ offset (car remaining-lengths))
                           :offset-list (cons offset offset-list))
      (let* ((result (reverse (cons offset offset-list)))
             (this-result-size (* multiplier (length result))))
        (mapcar (lambda (x) (+ x this-result-size))
                result))))
;;Target: 12 25 29.
(offsets-plus-endpos :remaining-lengths (mapcar #'length '("<h1>Auto</h1>" "Auto")))

(defun write-cluster (&key dir-entries stream-zim (compression-level 10))
  "If STREAM-ZIM is non-nil, write the content of a cluster to STREAM-ZIM. Otherwise, return an octet vector.
   The contents are derived from DIR-ENTRIES: if given, the file named in (relative-path dir-entry) is written, in the in-memory case, (content dir-entry) is written, the tile otherwise.
   DIR-ENTRIES: list of `dir-entry-content instances.
   If STREAM-ZIM is nil, an octet vector is returned. This is useful for parallelizing the compression, but keep in mind that too many parallel reads in the metatiles may overload the tile disk/source.
   If COMPRESSION-LEVEL is non-nil, the contents get compressed with zstd with the given level.
   This function is intended to be called after CLUSTER-ID BLOB-ID are enumerated - it's render time here."
  (let* ((write-directly? (and stream-zim (null compression-level)))
         (stream-cluster (if write-directly?
                             stream-zim
                             (flexi-streams:make-in-memory-output-stream)))
         (measurable-contents (loop for entry in dir-entries
                                    collect (cond ((typep entry 'dir-entry-file)
                                                   (absolute-path entry))
                                                  ((typep entry 'dir-entry-tile)
                                                   entry)
                                                  ((stringp (content entry))
                                                   (flexi-streams:string-to-octets (or (content entry) "") :external-format *flexi-utf8*))
                                                  (T (content entry))))))
    (when write-directly?
      ;;1→uncompressed 5→zstd
      (write-byte (if compression-level 5 1) stream-cluster))
    (dolist (offset (offsets-plus-endpos
                     :remaining-lengths (mapcar (lambda (x)
                                                  (cond ((pathnamep x)
                                                         (trivial-file-size:file-size-in-octets x))
                                                        ((typep x 'dir-entry-tile)
                                                         (size x))
                                                        (T (length x))))
                                                measurable-contents)))
      ;;4: offset size (8 for extended cluster otherwise).
      (write-integer offset 4 stream-cluster))
    (dolist (content measurable-contents)
      (cond ((pathnamep content)
             ;;Adapted from uiop.
             (with-open-file (i content :element-type '(unsigned-byte 8)
                                        :direction :input :if-does-not-exist :error)
               (uiop:copy-stream-to-stream i stream-cluster :element-type '(unsigned-byte 8))))
            ((typep content 'dir-entry-tile)
             (with-open-file (i (absolute-path-of-its-metatile content)
                                :element-type '(unsigned-byte 8)
                                :direction :input :if-does-not-exist :error)
               (file-position i (offset content))
               (write-sequence (lisp-binary:read-bytes (size content) i) stream-cluster)))
            (T (write-sequence content stream-cluster))))
    (unless write-directly?
      (let* ((payload (flexi-streams:get-output-stream-sequence stream-cluster))
             (potentially-compressed (if compression-level (zstd:compress-buffer payload :level compression-level) payload))
             ;;1→uncompressed 5→zstd
             (final-octets (concatenate '(simple-array (unsigned-byte 8) (*))
                                        (if compression-level #(5) #(1))
                                        potentially-compressed)))
        (if stream-zim
            (write-sequence final-octets stream-zim)
            final-octets)))))
;;Target: 0c000000190000001d0000003c68313e4175746f3c2f68313e4175746f.
(with-output-to-string (*standard-output*)
  (loop for byte across (subseq (flexi-streams:with-output-to-sequence (stream)
                                  (write-cluster :dir-entries (list (dir-entry :content "<h1>Auto</h1>") (dir-entry :content "Auto"))
                                                 :stream-zim stream :compression-level nil))
                                1)
        collect (format T "~(~2,'0X~)" byte)))


;;; Header

(defun header (&key (major-version 5) (minor-version 0) (uuid (random (ash 1 128))) (number-of-articles 0) (number-of-clusters 0)
                 (position-pointerlist-url 0) (position-pointerlist-title 0) (position-pointerlist-cluster 0) (position-mimelist 80)
                 (main-page-number 0) (position-checksum 0))
  "Returns the ZIM header as octet array."
  (flexi-streams:with-output-to-sequence (stream)
    ;;Magic number.
    (write-integer 72173914 4 stream)
    ;;Major version: 5.
    (write-integer major-version 2 stream)
    ;;Minor version: 0 (old namespace usage with - A I M etc.).
    (write-integer minor-version 2 stream)
    ;;UUID: 128 random bits.
    (write-integer uuid 16 stream)
    ;;Number of articles: 0 initially.
    (write-integer number-of-articles 4 stream)
    ;;Number of clusters: 0 initially.
    (write-integer number-of-clusters 4 stream)
    ;;Position of the directory pointerlist ordered by URL: 0 initially.
    (write-integer position-pointerlist-url 8 stream)
    ;;Position of the directory pointerlist ordered by title: 0 initially.
    (write-integer position-pointerlist-title 8 stream)
    ;;Position of the cluster pointer list: 0 initially.
    (write-integer position-pointerlist-cluster 8 stream)
    ;;Position of the MIME type list: 80. 80 is the file position after this #'header.
    (write-integer position-mimelist 8 stream)
    ;;Main page number: 0 initially.
    (write-integer main-page-number 4 stream)
    ;;Layout page number.
    (write-integer (1- (ash 1 32)) 4 stream)
    ;;Checksum position.
    (write-integer position-checksum 8 stream)))

