;;;; TOO (The Offline Oriented) creates ZIM files with offline available maps
;;;; Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>
;;;; 
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-user)

(defpackage too-test-asd
  (:use :common-lisp :asdf))

(in-package :too-test-asd)

(asdf:defsystem too-test
  :description "Describe too-test here."
  :author "Benedikt Steger"
  :license "AGPLv3+"
  :version "0.0.1"
  :serial T
  :depends-on (:asdf)
  :components ((:module "test"
                :serial T
                :components ((:file "package")))))

