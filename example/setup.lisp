;;;; TOO (The Offline Oriented) creates ZIM files with offline available maps
;;;; Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>
;;;; 
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

#|
    ╔════════════════════════════════════╗
    ║                                    ║
╔═══╝                                    ╚═══╗
║        A technical guide explaining        ║
║                                            ║
║                how to get a                ║
║                                            ║
║          → COMPLETE TOO ZIM FILE ←         ║
║                                            ║
║                   out of                   ║
║                                            ║
║            (ql:quickload :too)             ║
║          (too:flavour2zimfile),            ║
║                                            ║
║         how to execute these forms         ║
║                                            ║
║          ** FASTER THAN EVER™ **           ║
║                                            ║
║         and how to use them for a          ║
║                                            ║
║         !!! CUSTOMIZED RELEASE !!!         ║
║                                            ║
╚═══╗                  ❦                 ╔═══╝
    ║                                    ║
    ╚═══════════════╤═════╤══════════════╝
                    │ TOO │
                    └─────┘


Buy a computer with at least
 64GiB of RAM according to the osm2pgsql documentation. 128-192 GiB are recommended, though.
 a NVMe SSD. The database peaks at ~ 900 GiB during the clustering import step as of 2022-07.
 a CPU with more than one core.
 a screen large enough that allows for convenient code editing.
Get Debian 11 up and running (https://www.debian.org/).
Get a local tile cache up and running (https://switch2osm.org/).
 The planet file is available at https://planet.osm.org/.
 Make sure that you use font_dir=/usr/share/fonts in /etc/renderd.conf (the /usr/share/fonts/truetype value of the guide will lead to tofu).
 The default configuration of TOO expects tile sizes of 1024px, reachable under http://127.0.0.1/osm1024/{z}/{x}/{y}.png.

Read following books:
 Common Lisp: A Gentle Introduction to Symbolic Computation by David S. Touretzky (1990)
 Practical Common Lisp by Peter Seibel (2005)
  Available at http://gigamonkeys.com/book/index.html.
 Object-Oriented Programming in Common Lisp - A Programmer's Guide to CLOS by Sonya E. Keene (1989)
 Structure and interpretation of computer programs by Harold Abelson and Gerald Jay Sussman, with Julie Sussman (1996)
If not, at least read chapter 1 of Practical Common Lisp and Steve Losh's blog entry called "A Road to Common Lisp".

First and foremost, the development environment for Common Lisp programming needs to be set up (once). Open the console and type:

#TOO depends on libzstd1 (through Quicklisp package zstd).
sudo apt install sbcl sbcl-source emacs slime hyperdoc w3m libzstd1

Include following in $HOME/.emacs:
(setq slime-lisp-implementations
      '((sbcl ("/usr/bin/sbcl" "--dynamic-space-size" "23456" "--control-stack-size" "23456") :coding-system utf-8-unix)))

#Think before you type.
rm -R $HOME/quicklisp

mkdir $HOME/quicklisp

wget https://beta.quicklisp.org/quicklisp.lisp

wget https://beta.quicklisp.org/quicklisp.lisp.asc

gpg --verify quicklisp.lisp.asc quicklisp.lisp

#The repository mirroring step is optional (#'ensure-local-archive-file). 500MiB network traffic for an offline repository.
sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(ql:add-to-init-file)" --eval "(mapcar #'ql-dist:ensure-local-archive-file (ql-dist:provided-releases t))" --eval "(quit)"

cp -R /path/to/too $HOME/quicklisp/local-projects/

emacs -nw $HOME/quicklisp/local-projects/too/example/setup.lisp

Press Alt-x and enter "slime", followed by a carriage return (M-x slime RET). Switch back to this file with C-x b RET (CRTL-x then b then return).

Press C-c C-c (CTRL-c then CTRL-c) on the forms below. 
|#

(ql:quickload :too)

(defpackage :workbench
  (:use :cl :too))

(in-package :workbench)

(osm2pgsql-sql-commands)

#|
Compare the created file /dev/shm/temp to your database indices. Create the missing indices.

Let's register the flavours of a new release:
|#

(register-release-flavours
 ;;osmium fileinfo -e planet-??????.osm.pbf
 :planet (make-instance 'planet-file
                        :filesize 70417833793
                        :year 2022 :month 10 :day 3
                        :nodes 7950120815 :ways 891190805 :relations 10285396)
 :region (make-instance 'region
                        :nickname "earth"
                        :user-facing-name "the whole earth"
                        :geometry (make-instance 'mbr-4326 :ll-lat -90 :ll-lon -180
                                                           :ur-lat 90 :ur-lon 180))
 :zoom-mini 9 :zoom-maxi 14)

(defparameter *selected-flavour* (nth 11 *flavours*)
  "A handy shortcut for the running example.")

#|
The next form populates /dev/shm/temp with the commands that will bulk-render the needed tiles.

Setting "ModTileBulkMode" to "On" in /etc/apache2/conf-available/renderd.conf and ensuring that a planet timestamp resides in the tile dir (touch --date 20?????? /path/to/mod_tile/planet-import-complete) prevents expensive rerenders of unchanged database data.
|#

(render-list-mbr-commands
 :release-or-flavour *selected-flavour*
 :economically-use-ram? nil
 :splice-options (list "--all"
                       "--tile-dir=/mnt/main/mod_tile/"
                       (format nil "--map=osm~A" (tile-size *selected-flavour*))))

#|
After you have rendered all tiles, it's time to generate the ZIM file of a flavour.
|#

(unless *connection*
  (setf *connection* (cl-postgres:open-database "gis" "postgres" "abc123" "127.0.0.1" 5432)))

#|
The following form is the final one. It has finished when the [workbench sbcl 1] below becomes [workbench sbcl] again.

If you're interested in reproducibility, you've got to save this file (C-x C-s) before executing the following form.
|#

(when *connection*
  (time (flavour2zimfile
         :flavour *selected-flavour*
         :output-dir "/dev/shm/"
         :metatile-dir-base "/mnt/main/mod_tile/osm1024/"
         :compression-level 22
         :standalone? nil
         :force-local-server? nil)))

#|
Done!

zsync files (Debian package zsync) enable users to download differences to their existing files only. Additionally, zsync files work across all possible combinations of release dates, release regions, flavour differences, etc. Typically, zsync files are generated automatically, but it is also possible to produce them manually by typing zsyncmake osm-......zim for each flavour.

Users can then use zsync -i osm...old.zim -i osm...another-old.zim -i etc.zim http://example.com/osm-...zim.zsync.

Use zimsplit (in the Debian package zim-tools) if you want to provide splitted ZIM files.

See test/package.lisp for the development/testing procedure.
|#

