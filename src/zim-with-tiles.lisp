;;;; TOO (The Offline Oriented) creates ZIM files with offline available maps
;;;; Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>
;;;; 
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :too)

;;;; Write the ZIM file

(defun uuid (flavour &optional render?)
  "An integer that represents the ID of a flavour. Encodes YEAR MONTH DAY ZOOM-MAX #'final-nickname from FLAVOUR."
  (let ((value (parse-integer (format nil "~{~A~}"
                                      (loop for integer across (md5:md5sum-string
                                                                (format nil "This is a TOO flavour with :date ~A-~A-~A :maxzoom ~A :region ~A."
                                                                        (year flavour) (month flavour) (day flavour) (zoom-max flavour)
                                                                        (final-nickname flavour)))
                                            collect (format nil "~2,'0X" integer)))
                              :radix 16)))
    (if render?
        (with-output-to-string (out)
          (let ((stream (flexi-streams:make-in-memory-output-stream)))
            (write-integer value 16 stream)
            (loop for i = 0 then (incf i)
                  for byte across (flexi-streams:get-output-stream-sequence stream)
                  when (find i '(4 6 8 10))
                    do (format out "-")
                  do (format out "~(~2,'0X~)" byte))))
        value)))
;;(uuid workbench::*selected-flavour* T)

(defun contents2zimfile (&key (zim-contents *zim-example*) flavour (out-file "/dev/shm/test.zim") (metatile-dir-base "/var/lib/mod_tile/") (compression-level 22) (standalone? T))
  "Writes a ZIM file with ZIM-CONTENTS (the output of #'zim-contents - an instance of `dir).
   If COMPRESSION-LEVEL is non-nil, the clusters within the ZIM file get compressed with ZSTD.
   Map tiles are sourced directly from the metatiles in the map tile directory (if STANDALONE? is non-nil).
    (geometry FLAVOUR) defines what tiles are copied.
   This function isn't necessarily limited to TOO's use case. It is possible to leave FLAVOUR empty. No map tiles are added to the ZIM file in this case.
   Assumes A/index.html as the welcome page."
  (let* ((cluster-pointer-list ())
         (position-pointerlist-cluster nil)
         (dir-entry-pointer-list ())
         (position-pointerlist-url nil)
         (position-pointerlist-title nil)
         (position-checksum nil))
    (with-open-file (f out-file
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :direction :output
                       :element-type '(unsigned-byte 8))
      ;;Write a dummy header.
      (write-sequence (header) f)
      
      ;;The MIME type list must directly follow the header.
      (write-sequence (mime-type-list) f)
      
      (let* ((entries (enumerate-cluster-blobs!
                       :remaining-dir-entries (prepend-mime-histogram
                                               (append (flatten-dir zim-contents)
                                                       (when (and flavour standalone?)
                                                         (dir-entry-tiles
                                                          :flavour flavour
                                                          :metatile-dir-base metatile-dir-base)))))))
        
        ;;Write the cluster contents.
        ;;TODO: parallelize compression.
        ;;       The real TODO is "serialize parallel workers" actually, falling back to one worker atm.
        ;;        Already a measurable improvement indeed.
        ;;       lparallel's #'pmapcar, for example, is too memory expensive unfortunately...
        (let ((clusters (loop with buckets = (make-hash-table)
                              for entry in entries
                              when (typep entry 'dir-entry-content)
                                do (push entry (gethash (cluster-id entry) buckets))
                              finally (return (loop for v being the hash-value of buckets
                                                    collect (reverse v)))))
              (worker (legion:make-worker (lambda (cluster)
                                            (push (file-position f) cluster-pointer-list)
                                            (if (find-if (lambda (x) (typep x 'dir-entry-tile))
                                                         cluster)
                                                (write-cluster
                                                 :dir-entries cluster
                                                 :stream-zim f
                                                 :compression-level nil)
                                                (let ((payload (write-cluster
                                                                :dir-entries cluster
                                                                :compression-level compression-level)))
                                                  (write-sequence payload f)))))))
          (legion:start worker)
          (loop while clusters
                do (if (and clusters (< (legion:worker-queue-count worker) 10))
                       (legion:add-job worker (pop clusters))
                       ;;(sleep 0.001)
                       T))
          (legion:stop worker)
          (loop while (not (eql :shutdown (legion:worker-status worker)))
                do (sleep 0.001)))
        
        ;;The cluster pointers are known now.
        (setf position-pointerlist-cluster (file-position f))
        (dolist (cluster-pointer (reverse cluster-pointer-list))
          (write-integer cluster-pointer 8 f))
        
        (let ((resolved-entries-order-by-url
                (resolve-redirects!
                 (sort entries
                       #'string<
                       ;;Knowingly omitting #\/.
                       :key (lambda (x) (concatenate 'string (namespace x) (url x)))))))
          
          ;;Write all directory entries since the `dir-entry-content instances have a populated cluster-id blob-id now, ...
          (dolist (entry resolved-entries-order-by-url)
            (push (file-position f) dir-entry-pointer-list)
            (write-dir-entry entry f))
          
          ;;... and just got a position.
          (setf position-pointerlist-url (file-position f))
          (dolist (dir-entry-pointer (reverse dir-entry-pointer-list))
            (write-integer dir-entry-pointer 8 f))
          
          ;;Title index.
          (setf position-pointerlist-title (file-position f))
          (mapcar (lambda (x) (write-integer (cdr x) 4 f))
                  (sort (pairlis resolved-entries-order-by-url
                                 (loop for i from 0 below (length resolved-entries-order-by-url) collect i))
                        #'string<
                        :key (lambda (x)
                               (let ((entry (car x)))
                                 ;;Knowingly omitting #\/.
                                 (concatenate 'string (namespace entry) (or (title entry) (url entry)))))))
          
          (setf position-checksum (file-position f))
          
          ;;Rewrite the header with the now-known positions.
          (file-position f 0)
          (write-sequence (header
                           :uuid (if flavour
                                     ;;Stable hash for reproducibility.
                                     (uuid flavour)
                                     (random (ash 1 128)))
                           :number-of-articles (length resolved-entries-order-by-url)
                           :number-of-clusters (length (remove-duplicates (loop for entry in resolved-entries-order-by-url
                                                                                when (typep entry 'dir-entry-content)
                                                                                  collect (cluster-id entry))))
                           :position-pointerlist-cluster position-pointerlist-cluster
                           :position-pointerlist-url position-pointerlist-url
                           :position-pointerlist-title position-pointerlist-title
                           :position-checksum position-checksum
                           :main-page-number (resolve "A/index.html" resolved-entries-order-by-url))
                          f))))
    
    ;;Compute the checksum and append it.
    (let ((md5 (md5:md5sum-file out-file)))
      (with-open-file (f out-file
                         :if-exists :append
                         :if-does-not-exist :error
                         :direction :output
                         :element-type '(unsigned-byte 8))
        (write-sequence md5 f)))
    (format nil "ZIM file ~A written successfully." out-file)))
;;(contents2zimfile :zim-contents *zim-example*)
;;(contents2zimfile :zim-contents *zim-example* :flavour *selected-flavour*)

