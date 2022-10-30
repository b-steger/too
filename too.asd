;;;; TOO (The Offline Oriented) creates ZIM files with offline available maps
;;;; Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>
;;;; 
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-user)

(defpackage too-asd
  (:use :common-lisp :asdf))

(in-package :too-asd)

(asdf:defsystem too
  ;;CACHE CONTROL: Authority: README.md.
  :description "TOO (The Offline Oriented) creates offline available map files. The output files adhere to the ZIM format and can therefore be opened by ZIM readers such as the Kiwix browser. The generated ZIM files include Leaflet for map display, cache prerendered map tiles, store a selection of layers that display 500k features reasonably fast through clustering, and contain a disambiguated place name index which resolves place names to 1. points and 2. MBRs for non-point sources."
  :author "Benedikt Steger <b.steger@protonmail.ch>"
  :license "AGPLv3+"
  ;;CACHE CONTROL: Authority: too::*versions*.
  :version "1.0"
  :serial T
  :depends-on (:asdf :cl-fad :cl-postgres :closer-mop :flexi-streams :lisp-binary :md5 :trivial-file-size :zip :zstd :legion)
  :components ((:module "src"
                :serial T
                :components ((:file "package")
                             (:file "common")
                             (:file "lisp-at-work")
                             (:file "uncharter" :depends-on ("lisp-at-work"))
                             (:file "osm2pgsql-indexing" :depends-on ("common" "lisp-at-work"))
                             (:file "zim-general")
                             (:file "map-tiles" :depends-on ("zim-general"))
                             (:file "zim-with-tiles" :depends-on ("zim-general" "map-tiles"))
                             (:file "unresolutions" :depends-on ("lisp-at-work" "zim-general"))
                             (:file "opfff")
                             (:file "wiki-definition")
                             (:file "layer" :depends-on ("wiki-definition"))
                             (:file "release-related")
                             (:file "geocode")
                             (:file "user-interface")))))

;;jubako.lisp

