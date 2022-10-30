;;;; TOO (The Offline Oriented) creates ZIM files with offline available maps
;;;; Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>
;;;; 
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :too)

;;;; Dump the initial contents of the ZIM file of a flavour of the TOO project.

#|
Design choice: all possible combinations of layers are materialized. This is needed since low-end devices may crash otherwise: they do not have the memory to hold all layer data. Others will have to wait for layers they possibly to not need.
 This is perhaps only relevant for the earth region release.
  It is ok to keep this approach for releases with smaller layers since the full-fledged map is only two steps from the welcome file (Multiple layers → Continue with all ...).
The space usage of the map files themselves may be worth a consideration, since (expt 2 number-of-layers) files have to be generated.
 This is also on an acceptable level with 14 layers for the earth region release.
 Development, on the other hand, may be inconvenient, as the REPL is stretched by some few seconds.
  Just comment out some layers in release-related.lisp in such a case.

TODO:
 Tiles without labels + label layer.
  Or better yet: vector tiles + WebGL. → Battery life? Device/GPU support? The pro arguments are clear...
 One lv (group POIs into one cluster overlay).
  This is actually debatable since the clustering of features needs to be recalculated each time a layer is activated. Is the battery argument valid and important in this context?
|#

;;;; Content

(defparameter *versions* (list :too "1.0"
                               :leaflet "1.9.2"
                               :prunecluster "2.1.0"
                               :geolet "20.12.31")
  "Versions of the software that is included in the ZIM files.")

(defun version (software)
  "Looks up the version of SOFTWARE (a keyword) in *versions*."
  (getf *versions* software))
(version :too)

(defparameter *zim-style* (x-style "table,tr,th,td{border:1px solid #777;border-collapse:collapse;vertical-align:top;text-align:center}"
                                   (dsl nil
                                        ".menuitem"
                                        (dsl "{;}"
                                             ;;fcfff6
                                             "background-color:#f4f4f4"
                                             "float:left"
                                             "border:3px solid #969696"
                                             "border-radius:0.5em"
                                             "box-shadow:0px 0px 5px #969696"
                                             "padding:1em"
                                             "margin:0.5em"
                                             "text-align:center"
                                             "font-weight:bold"
                                             "text-decoration:none"))
                                   ".menuitem:hover{background-color:#fcfcfc;border-color:#999999}"
                                   "table{border:3px solid #969696}"))

(defun background (&optional (levels-up 0))
  "HTML style tag for the background used in HTML pages."
  (list :style
        (format nil "background-image:url('~{~A~}../I/background_cropped_bright_navigable.png')"
                (loop for i from 0 below levels-up collect "../"))))
(background 2)

(defun back-to-welcome-page (&optional (levels-up 0))
  (x-p (x-a :href (dsl nil
                       (loop for i from 0 below levels-up collect "../")
                       "index.html")
            ;;In order to work around kiwix-android's URL truncation behaviour, locator articles make use of an iframe in order to transport information. It is ok to remain within the locator article when choosing new layers. But it is not ok when the locator article is of no further interest.
            :target "_parent"
            "← Back to the welcome page")))

(defun copyright-file ()
  "A/legal/index.html"
  (let ((title "Legal notices"))
    (dir-entry
     :url "index.html"
     :title title
     :mimetype-id :html
     :content (dsl '("<!DOCTYPE html>" nil "</html>")
                   (x-head (x-title title)
                           "<meta charset=\"utf-8\">"
                           "<style>li { margin-top:0.5em;margin-bottom:0.5em }</style>")
                   (x-body
                    (background 1)
                    (back-to-welcome-page 1)
                    (x-h2 "Legal notices")
                    (x-ul (x-li (dsl nil
                                     "The map and layer data are sourced from the OpenStreetMap™ database, which is "
                                     (x-a :href "Copyright.html" "Copyright")
                                     " OpenstreetMap™ "
                                     (x-a :href "Contributors.html" "contributors")
                                     " and licensed under the "
                                     (x-a :href "odbl-10.html" "Open Database License 1.0")
                                     "."))
                          (x-li "The TOO project is not affiliated with the OpenStreetMap Foundation.")
                          (x-li "The <code>convertBase</code> function that is used in <code>map.js</code> and the rendered tiles are licensed under "
                                (x-a :href "./CC-BY-SA-4.0.html" "Creative Commons - Attribution - Sharealike version 4.0")
                                ".")
                          (x-li "The symbols, which are adapted from maki-icons and Temaki, are released under the "
                                (x-a :href "./CC0-1.0.html" "CC0 Public Domain Dedication, version 1.0")
                                ", as published by Creative Commons.")
                          ;; and its <a href='../leaflet-~A/README.html'>readme file</a> are  (version :leaflet)
                          (x-li (format nil "The map interface is powered by the JavaScript library Leaflet. Leaflet is Copyright (c) 2010-2022 Vladimir Agafonkin, (c) 2010-2011 CloudMade and licensed under the <a href=\"./BSD-2-Clause-Leaflet.html\">BSD 2-clause License</a>.")
                                (x-ul (x-li "The URL hash is created with the Leaflet plugin leaflet-hash and Copyright (c) 2013 Michael Lawrence Evans, as noted in the <a href=\"./MIT-leaflet-hash.html\">MIT license file</a>, under which leaflet-hash is licensed.")
                                      (x-li "Clustering is powered by the Leaflet plugin PruneCluster and Copyright (c) 2014 SINTEF-9012. PruneCluster is licensed under the <a href=\"./MIT-PruneCluster.html\">MIT License</a>.")
                                      (x-li "The geolocation button is powered by the Leaflet plugin GEOLET and Copyright (c) 2020 Ruben Holthuijsen. GEOLET is licensed under the <a href=\"./MIT-GEOLET.html\">MIT License</a>.")))
                          (x-li "In accordance to <a href='https://undocs.org/ST/AI/189/Add.9/Rev.2'>UN Administrative Instruction ST/AI/189/Add.9/Rev.2</a>, the <a href='../un/uncharter.html'>Charter of the United Nations</a> and the resolutions <a href='../un/A_RES_68_262-EN.html'>A/RES/68/262</a>, <a href='../un/A_RES_ES-11_1-EN.html'>A/RES/ES-11/1</a>, <a href='../un/A_RES_ES-11_2-EN.html'>A/RES/ES-11/2</a>, <a href='../un/A_RES_ES-11_3-EN.html'>A/RES/ES-11/3</a> and <a href='../un/A_RES_ES-11_4-EN.html'>A/RES/ES-11/4</a> are in the public domain worldwide.")
                          (x-li "Other files or file contents in this ZIM file are Copyright 2022 Benedikt Steger and licensed under the "
                                (x-a :href "./AGPLv3.html" "GNU Affero General Public License version 3 or later (AGPLv3)")
                                ". They are part of or built by the Common Lisp library <a href=\"../too.zip\">TOO</a>, which is released under the AGPLv3+.")
                          (x-li "The services provided by the TOO project are purely provided on a best-effort basis and may be limited by time constraints of the volunteering project members."))
                    (x-h3 "Map disclaimer")
                    (x-p "The designations employed and the presentation of material on the included map do not imply the expression of any opinion whatsoever on the part of the publisher(s) of this ZIM file concerning the legal status of any country, territory, city or area or its authorities, or concerning the delimitation of its frontiers or boundaries.")
                    (x-h3 "Warranty disclaimer")
                    (x-p "THIS ZIM FILE IS DISTRIBUTED &apos;AS IS&apos;, AND IN THE HOPE THAT IT WILL BE USEFUL, BUT WITHOUT WARRANTY OF ANY KIND, EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT. IN NO EVENT WILL THE PUBLISHER(S) OF THIS ZIM FILE BE LIABLE FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES RESULTING FROM ANY INACCURACY OR ERROR IN THIS ZIM FILE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGES."))))))

(defun sanitize-to-letters (a-string)
  "Replace each non-letter with an hyphen."
  (format nil "~{~A~}"
          (loop for char across a-string
                collect (if (or (char<= #\a char #\z)
                                (char<= #\A char #\Z))
                            char
                            #\-))))
(sanitize-to-letters "amenity=hospital (abc)")

(defun wiki-article-file (article)
  "Render ARTICLE, an instance of `wiki-definition, to an instance of `dir-entry."
  (let ((title (format nil "Small Wiki: ~A" (name article))))
    (dir-entry
     :url (format nil "~A.html" (sanitize-to-letters (name article)))
     :title title
     :mimetype-id :html
     :content (dsl '("<!DOCTYPE html>" nil nil)
                   (x-html
                    (x-head (x-title title)
                            "<meta charset=\"utf-8\">")
                    (x-body
                     (background 1)
                     (back-to-welcome-page 1)
                     (x-h2 title)
                     (loop for p in (paragraphs article)
                           collect (x-p p))))))))

(defun zim-file-name (final-nickname 256px-zoom-max year month)
  (format nil "too-~(~A~)-~2,'0D-~A_~A-~2,'0D.zim"
          final-nickname 256px-zoom-max (zoom-nickname 256px-zoom-max) year month))
(zim-file-name "Earth" 14 2022 07)

(defun menuitem (link-text title url image-source)
  "A menu item with an image."
  (x-p (x-a :class "menuitem"
            :href url
            (when title (list :title title))
            (x-img :src image-source
                   :style "max-width:48px;height:48px")
            (x-br)
            link-text)))

(defparameter *osm-scales* '(591659008 295829504 147914752 73957376 36978688 18489344 9244672 4622336 2311168 1155584 577792 288896 144448 72224 36112 18056 9028 4514 2257 1128 564))

(defun osm-scales-file ()
  "osm-scales.xml"
  (dir-entry
   :url "map-scales.xml"
   :title "QGIS-loadable list of predefined scales used by TOO"
   :mimetype-id :xml
   :content (x-qgsscales :version "1.0"
                         (loop for scale in *osm-scales*
                               collect (x-scale :value (format nil "1:~A" scale))))))
(osm-scales-file)

(defun index-file (&key flavour num-loc-files)
  "index.html"
  (labels ((readable-month (month)
             (case month
               (1 "January") (2 "February") (3 "March") (4 "April")
               (5 "May") (6 "June") (7 "July") (8 "August")
               (9 "September") (10 "October") (11 "November") (12 "December")))
           (version-paragraph (finishing-punctuation)
             (x-p "This file is based on a database dump from "
                  (x-b (format nil "~A-~A-~2,'0D" (year flavour) (readable-month (month flavour)) (day flavour)))
                  " and covers " (x-b (user-facing-name flavour)) " up to zoom level "
                  (x-b (zoom-max flavour))
                  finishing-punctuation)))
    (let ((layers (layers flavour))
          (title "The Offline Oriented (TOO)"))
      (dir-entry
       :url "index.html"
       :title title
       :mimetype-id :html
       :content
       (dsl '("<!DOCTYPE html>" nil nil)
            (x-html
             (x-head (x-title title)
                     *zim-style*
                     "<meta charset=\"utf-8\">")
             (x-body
              (background 0)
              (x-h1 title)
              ;;(menuitem "Base map" "./layer-selection.html" "./osmlogo48x48.png")
              (menuitem "Base map" "A general reference map"
                        (format nil "./layer-selection/~{~A~}.html"
                                (if (< 0 (length layers))
                                    (loop for i from 0 below (length layers) collect "0")
                                    '("0")))
                        "../I/TOO-Logo-without-marker.png")
              #||(x-p (format nil "<a class='menuitem' href='../A/layer-selection/~B.html'>Map with all layers (possibly slow initially)</a>"
              (1- (ash 1 (length layers)))))|#
              (menuitem "Load a layer" nil "#layer-list" (format nil "../-/leaflet-~A/images/marker-icon-2x.png" (version :leaflet)))
              (menuitem "Multiple layers" nil "./layer-selection.html" (format nil "../-/leaflet-~A/images/layers-2x.png" (version :leaflet)))
              (x-p "<a class='menuitem' href='./name-lookup.html'><span style='font-size:40px;margin-top:0em;font-weight:bold;color:#333'>A<sup>B</sup>C</span><br>Name lookup</a>")
              #|(x-p (format nil "<a class='menuitem' href='../-/layer-selection/~B.html'>DEV</a>"
              (subseq (format nil "~B" (1+ (ash 1 (length layers)))) 1)))|#
              (x-p "<a class='menuitem' href='./legal/index.html'><span style='font-size:46px;margin-top:0em;font-weight:bold;color:#000'>§</span><br>Legal notices</a>")
              "<br clear='both'>"
              (x-h2 "About")
              (x-p "The Offline Oriented (TOO) is a map in a ZIM file. TOO contains some useful layers and a disambiguated place name index. Consult the <a href='./legal/index.html'>legal notices</a> for an exact breakdown of TOO's components and their licenses.")
              (version-paragraph ".")
              (x-h2 "Zoom level selection")
              (x-p "Different flavours are released for each rendered region. The minimal flavours aim for file sizes around 1 GiB, whereas the maximal flavours may use as much as 100 GiB card space. For each rendered region and flavour, suitable maximal zoom levels are chosen.")
              (x-p "Following zoom levels are candidates for maximal zoom levels:")
              (x-table (x-tr (x-th "Zoom level") (x-th "ZIM-file nickname") (x-th "Description"))
                       (x-tr (x-td "0-8") (x-td "(not in ZIM file names)") (x-td "Low zoom levels."))
                       (loop for z being the hash-value of *zoom-levels*
                             collect (when (<= 9 (level z))
                                       (x-tr (x-td (level z))
                                             (x-td (nickname z))
                                             (x-td (description z))))))
              (version-paragraph ":")
              (x-table (x-tr (x-th "Flavours and their zoom levels") (loop for z from 0 upto 20 collect (x-th z)))
                       (loop for zoom-max in (release-max-zooms flavour)
                             collect (x-tr (x-td (zim-file-name (final-nickname flavour) zoom-max (year flavour) (month flavour))
                                                 (when (= zoom-max (zoom-max flavour))
                                                   " (this file)"))
                                           (loop for zoom from 0 upto 20
                                                 collect (x-td (if (find zoom (quick-flavour-zoom-levels zoom-max (tile-size flavour)))
                                                                   "yes"
                                                                   ""))))))
              (x-p "It is always a good idea to get the largest file if possible, because a higher zoom level flavour always contains more information than any lower zoom level. For example, a flavour with zoom level 14 will include the information from zoom level 13. Additionally, it is less likely that a label covers an area where a point of interest may be situated.")
              (x-p (format nil "The tiles below zoom level ~A are always included for the whole Earth, regardless of the ZIM file region in effect."
                           ;;1+: "below" leaves less room for interpretation than "up to".
                           (1+ *max-zoom-level-without-region-limits*)))
              (x-h2 "Layers")
              (x-p "All ZIM files include some layers. All matching features within the ZIM file region are included, whether they happen to be rendered in the included zoom levels or not, i.e. every flavour contains identical layer data. Note that this does not necessarily mean, as a matter of fact, that the available data sources are complete, it only means that the layers contain every matching feature from the data sources.")
              (x-table (x-tr (x-th "Flavour") (x-th "Layers contain everything intersecting") (x-th "Included layers") (x-th "Missing features?"))
                       (loop for zoom in (release-max-zooms flavour)
                             collect (x-tr (x-td (zim-file-name (final-nickname flavour) zoom (year flavour) (month flavour))
                                                 (when (= zoom (zoom-max flavour))
                                                   " (this file)"))
                                           (x-td (user-facing-name flavour))
                                           (x-td "All that are chosen for this region.")
                                           (x-td "Contains every matching feature from the data sources."))))
              (x-p "All layers contain all matching points, lines and polygons. The geometric representation used in the layers is always a point, namely, the closest intersecting point to the centroid of the geometry instance in question.")
              (x-p (x-a :href "./layer-selection.html" "Loading all or multiple layers at once")
                   " may take some seconds to load initially, depending on the involved hardware. If even a single layer alone is prohibitively slow on your device, feel free to provide feedback.")
              (x-a :name "layer-list" "")
              (x-h3 "Layer list")
              (x-table
               (x-tr (x-th "Name")
                     (x-th "Description")
                     (x-th "Fields in tooltip"))
               (loop for i = 0 then (incf i)
                     for layer in layers
                     collect (x-tr (x-td (x-a :style "text-decoration:none"
                                              :href (format nil "./layer-selection/~A.html" (toggle-layer-on i (layers flavour)))
                                              (x-img :src (format nil "../I/symbols/~A.svg" (name layer))
                                                     :style "width:3em;height:3em;margin:1em")
                                              (x-br :clear "both")
                                              (layer-name-0nf layer flavour)))
                                   (x-td (loop for p in (own-description-paragraphs layer)
                                               collect (x-p p))
                                         (wiki-list (wiki-definitions layer)))
                                   (x-td (when (columns layer)
                                           (dsl '(", ")
                                                (let ((names (mapcar #'user-facing-name (columns layer))))
                                                  (asplice
                                                   (string-capitalize (first names))
                                                   (rest names))))) "."))))
              (x-h2 "Feedback")
              (x-p "Both the development of the supporting software and these ZIM files are open. You are encouraged to give feedback.")
              (x-p "What layers are most helpful? What region should be rendered next? Is a yearly update interval enough or way too long? Are the filesizes too small/too big? Would you prefer to turn off the labels on the map? Are the layers too slow? Is a search index of layer features important? How do you work with the map? Would you like to measure on the map? Would you like to overlay external layer files? Is your device modern and powerful enough for WebGL? What is the production year of your device? Does your device run on batteries?")
              (x-p "Please provide feedback on the <a href='https://github.com/b-steger/too/issues'>issues page of TOO</a>, the software behind the project.")
              ;;TODO: bookmarklet which loads an online layer with full zoom levels.
              (x-h2 "Tips and tricks")
              (x-ul
               (x-li "Use the attribution link in the map to get back to this welcome file if the interface does not provide a link to this welcome file.")
               (x-li "If you want to add a place to your reading list in the Kiwix Desktop, you've got to empty the search field and subsequently click into the map in order to unlock the &quot;Add to the reading list&quot;-button.")
               (x-li "You don't have to click/tap on the blue/yellow/orange clusters, you can just continue zooming in.")
               (x-li "There is no need to close the popup via its X or its attached feature symbol - just click/tap somewhere else onto the empty map.")
               (x-li "Repeatedly pressing the plus button in the upper left corner is the fastest way to zoom into a freshly opened point search entry.")
               (x-li "If a feature symbol is getting in the way, you can either continue zooming in or temporarily turn off the layer through the layer selection in the upper right corner.")
               (x-li "If a feature isn't clearly locatable, for example due to a label or due to a low maximal zoom level in the ZIM file, you can try to infer its location with the help of features from other layers. The scale bar in the lower left corner may also be of help in such a case. Asking locals is yet another variant.")
               ;;TODO: measure RAM usage of popups with coordinates vs. popups without.
               (x-li "The map shows the coordinates of its center in the lower right corner. You can get a good approximation of the coordinates of a feature by zooming to its symbol up to the maximal zoom level 20. Most navigational devices support coordinates as the navigation target."))
              (x-h2 "Technical information")
              (x-ul
               (x-li (format nil "This ZIM file does not contain information newer than ~A. ~A ~A." (day flavour) (readable-month (month flavour)) (year flavour))
                     " A given zoom level contains 4<sup>zoom</sup> undeduplicated tiles and is essentially an image with a width and height of 256*2<sup>zoom</sup> pixels."
                     (format nil " The index of this ZIM file, which registers place names and their English translation, if available, contains ~A place-related entries." num-loc-files))
               (x-li "The exact SQL queries for the layers are prepended to the raw layer files.")
               ;;TODO: Kiwix on Android won't provide too.zip for download.
               (x-li "The purpose-built Common Lisp library <a href=\"too.zip\">TOO</a> is used for the creation of this ZIM file. TOO helps with database indices, gathers JavaScript files, the layer data and the map tiles, renders the navigation and place pages, and finally writes the contents to a ZIM file.")
               (x-li "A fresh map import takes 8 hours. Rendering zoom levels 0-13 lasts one day, rendering zoom level 14 takes two days and rendering zoom level 16 needs at least one week. The ZIM files are created within few hours. Overall, the whole process for a fresh Earth release with a maximal zoom level 14 in its biggest flavour takes about a week from the first byte downloaded to the last byte uploaded. The bottleneck in all steps is the I/O (input/output), even when the data is sorted along a space-filling curve or some spatial hash.")
               (x-li "All geometries intersecting the antimeridian, i.e. the great circle from the North pole to the South pole at 180°W = 180°E, are split by the antimeridian. This is due to a technical limitation which stems from a modulo-oblivious data model.")
               (x-li "For each flavour, zsync files are created. zsync allows you to reuse existing file contents of one or multiple old TOO ZIM files during the download (<code>zsync -i old-file1.zim -i old-file2.zim http://example.com/too....zim.zsync</code>). But note that you can use the content of old files in torrent programs, too. Just add the old file to a freshly started torrent. The torrent program will check the old file, and determine that a lot of blocks are invalid, but it will accept matching file contents. Most overlap is achieved between the files with the same region and included zoom levels. This means that flavours with uneven maximal zoom levels (z09, z11, z13, ...) have good deduplication properties within the group, as it is the case with the second group, the flavours with even maximal zoom levels (z10, z12, z14, ...).")
               (x-li "For requirements other than browsing, use zimdump, which is available in the Debian package zim-tools, to get the tile cache and layers. Make sure that you use an operating system that supports the characters <code>! $ ( ) * + , - . _</code> in the filename. You can also start a local Kiwix server or Kiwix hotspot and use this ZIM file as a tile server for another interface: just copy an image URL in the map and replace the last three separated numbers with <code>{z}/{x}/{y}</code>. The tiles are using the EPSG:3857 (web mercator) projection."
                     (x-h3 "TOO and QGIS")
                     "You can include the base map as a background map in QGIS."
                     (x-h4 "General considerations")
                     (x-p "First and foremost, make sure that the local Kiwix server or Kiwix hotspot is started. Open this welcome page in the browser and verify in a new tab that the base map shows up.")
                     (x-p "This ZIM file only stores " (if (evenp (zoom-min flavour)) "even" "odd") " zoom levels. Go to <code>Settings</code> → <code>Options</code> → <code>Map Tools</code> → <code>Zooming</code> and set the <code>Zoom factor</code> to &quot;400%&quot; in order to skip the missing zoom levels.")
                     (x-p "You can also set the predefined map scales in the same dialog. Just download and add <a href='map-scales.xml'>map-scales.xml</a> to <code>Predefined scales</code>. Alternatively, you can enter " (elt *osm-scales* (zoom-min flavour)) " or &quot;1:" (elt *osm-scales* (zoom-min flavour)) "&quot; in the scale text field in the status bar.")
                     (x-h4 "Adding the map to QGIS (method 1)")
                     (x-ol (x-li "Copy an image URL and replace the last three separated numbers with <code>${z}/${x}/${y}</code>.")
                           ;; for the GDAL WMTS client driver
                           (x-li "Compare the <code>ServerUrl</code> in the following local service description XML file to the tile URL from the browser. Adapt the <code>ServerUrl</code> if needed."
                                 (x-form (x-textarea :id "gdalwms"
                                                     :style "width:90%;height:10em"
                                                     (escape-xml (x-gdal_wms
                                                                  (x-service :name "TMS"
                                                                             #\Newline #\Newline 
                                                                             (x-serverurl "NOJAVASCRIPTACTIVATED")
                                                                             #\Newline #\Newline #\Newline)
                                                                  (x-layer (format nil "too_~A" (uuid flavour T)))
                                                                  ;;THINK: prevent "underzooming".
                                                                  (x-zoomlevel (as-non-256px-zoom (zoom-max flavour) (tile-size flavour)))
                                                                  (x-datawindow
                                                                   (x-upperleftx "-20037508.34")
                                                                   (x-upperlefty "20037508.34")
                                                                   (x-lowerrightx "20037508.34")
                                                                   (x-lowerrighty "-20037508.34")
                                                                   ;;Allow overzooming.
                                                                   (x-tilelevel (as-non-256px-zoom (zoom-max flavour) (tile-size flavour)))
                                                                   (x-tilecountx 1)
                                                                   (x-tilecounty 1)
                                                                   (x-yorigin "top"))
                                                                  (x-projection "EPSG:3857")
                                                                  (x-blocksizex (tile-size flavour))
                                                                  (x-blocksizey (tile-size flavour))
                                                                  (x-bandscount 3)
                                                                  ;;Next 2: avoid black tiles. Transparency/no tile is better.
                                                                  ;;(x-zeroblockonserverexception "true")
                                                                  ;;THINK: redirects? Redirects work with kiwix-desktop kiwix-serve, if not in QGIS kiwix-android.
                                                                  ;;(x-zeroblockhttpcodes "204,404")
                                                                  ;;Self-signed certificates are quite possible in offline settings.
                                                                  (x-unsafessl "true")))))
                                 (x-script (dsl '("document.getElementById('gdalwms').innerHTML=document.getElementById('gdalwms').innerHTML.replace('NOJAVASCRIPTACTIVATED'," ")")
                                                (jsq-ternary "document.location.protocol.startsWith('http')"
                                                             (format nil "document.location.protocol+'//'+document.location.host+'/~A/I/t/${z}/${x}/${y}.png'" (uuid flavour T))
                                                             (dsl :squote "PLEASE OPEN THE WELCOME PAGE IN THE BROWSER")))))
                           (x-li "Save the contents to a file named <code>too.xml</code>.")
                           (x-li "Open QGIS, open the <code>Layer</code> menu and open the Data Source Manager (Ctrl-L).")
                           (x-li "Go to <code>Raster</code> → <code>Source</code> and open the XML file.")
                           (x-li "Click on <code>Add</code>.")
                           (x-li "Enter " (elt *osm-scales* (zoom-min flavour)) " in the scale text field in the status bar."))
                     (x-h4 "Adding the map to QGIS (method 2)")
                     (x-p "This method is a fallback method that you should only consider when the first method failed.")
                     (x-ol (x-li "Copy an image URL and replace the last three separated numbers with <code>{z}/{x}/{y}</code>.")
                           (x-li "Create a new XYZ Tiles connection")
                           (x-li (format nil "Paste the URL, set the minimal zoom level to ~A, the maximal zoom level to ~A and the tile resolution to <code>Standard (256x256 / 96 DPI)</code>." (zoom-min flavour) (zoom-max flavour))
                                 (x-ul (x-li (x-script
                                              (dsl '("document.write(" ")")
                                                   (jsq-ternary "document.location.protocol.startsWith('http')"
                                                                (dsl "+"
                                                                     (dsl :squote "The URL should look similar to <code>")
                                                                     "document.location.protocol"
                                                                     (dsl :squote "//")
                                                                     "document.location.host"
                                                                     (dsl :squote (format nil "/~A/I/t/{z}/{x}/{y}.png" (uuid flavour T)))
                                                                     ;;Extra no #\..
                                                                     (dsl :squote "</code>"))
                                                                (dsl :squote "Please start the Kiwix server or Kiwix hotspot now."))))))
                                 (x-li (format nil "In the lower right corner, set the Magnifier level to ~A%." (* 100 (expt 2 (- (zoom-offset 1024))))))
                                 (x-li "Drag the newly created entry into the Layers Panel.")
                                 (x-li "Right-click on the newly created layer and choose &quot;Zoom to Native Resolution (100%)&quot;."))))))))))))

(defun toggle-layer-on (toggling-layer-position layers)
  "Combination with a single layer at TOGGLING-LAYER-POSITION set to 1, printed as binary string (as used in the layer-selection folder)."
  (let ((number-of-layers (length layers)))
    (assert (< toggling-layer-position number-of-layers))
    (format nil (format nil "~~~A,'0B" number-of-layers) (ash 1 toggling-layer-position))))
(let ((layers (layers (car *flavours*))))
  (when (< 1 (length layers)) (toggle-layer-on 1 layers)))

(defun suppress-kiwix-interface ()
  "JavaScript code that suppresses kiwix-serve's interface if information is present in the URL.
   Must be placed within the HTML body."
  (x-script :type "text/javascript"
            ;;location: don't move the controls under kiwix-serve's toolbar (not if filtering, and not if the search is empty).
            (dsl '("if(document.location.search.match('location')){" ";" "}")
                 "document.body.style.setProperty('padding-top','0em','important')"
                 "document.body.getElementsByClassName('kiwix')[0].style.display='none'")))

(defun layer-selection-file (flavour)
  "layer-selection.html"
  (let* ((title "What layers should be selectable?"))
    (dir-entry
     :url "layer-selection.html"
     :title title
     :mimetype-id :html
     :content
     (dsl '("<!DOCTYPE html>" nil nil)
          (x-html
           (x-head (x-title title)
                   *zim-style*
                   "<meta charset=\"utf-8\">"
                   (x-script :type "text/javascript"
                             (dsl ";"
                                  (dsl "="
                                       "window.onsubmit"
                                       (dsl '("function (e) {" ";" "}")
                                            "e.preventDefault()"
                                            (dsl "="
                                                 "window.location.href"
                                                 (dsl "+"
                                                      (dsl :squote "layer-selection/")
                                                      (reverse
                                                       (loop for i = 0 then (incf i)
                                                             for layer in (layers flavour)
                                                             collect (let ((id (format nil "layer~A" i)))
                                                                       (jsq-ternary (format nil "document.getElementById('~A').checked" id)
                                                                                    "'1'"
                                                                                    "'0'"))))
                                                      (dsl :squote ".html")
                                                      "window.location.search")))))))
           (x-body
            (append (background 0)
                    ;;Pass-through markers.
                    '((:onload "document.getElementById('all').href=document.getElementById('all').href+window.location.search")))
            (suppress-kiwix-interface)
            (back-to-welcome-page)
            (x-h2 title)
            (x-p "A map with multiple layers may load slowly since it has to parse all involved layers, even when they are not shown. Reduce resource utilization and load times by limiting the selectable layers.")
            (x-a :href (format nil "layer-selection/~B.html" (1- (ash 1 (length (layers flavour)))))
                 :style "padding:0.3em;font-weight:bold;font-size:1.25em"
                 :id "all"
                 "→ Continue with all layers →")
            (x-p (x-i "May load slowly."))
            (x-hr :style "margin-top:1.5em;margin-bottom:1.5em")
            (when (< 0 (length (layers flavour)))
              (x-form
               :action "?"
               (loop for i = 0 then (incf i)
                     for layer in (layers flavour)
                     collect (let ((id (format nil "layer~A" i)))
                               (x-div (x-input :style "margin:0.5em"
                                               :type "checkbox"
                                               :id id)
                                      (x-label :for id
                                               :style "margin:0.5em"
                                               (layer-name-0nf layer flavour T)))))
               (x-input :type "submit"
                        :value "Open the faster map"
                        :style "margin-top:1.5em;margin-bottom:1.5em;font-weight:bold")))
            (x-p (x-b "Hint:")
                 " each selection is represented in a separate map file. You can add your selection in the (ZIM-)browser to the reading list (bookmarks). If you start a server (Ctrl-I in Kiwix Desktop), it even becomes possible to bookmark the maps with an initial zoom level and position.")))))))

(defun map-file-leaflet-css ()
  "map.css"
  (dir-entry
   :namespace "-"
   :url "map.css"
   :mimetype-id :css
   :content
   (dsl nil
        ".leaflet-tile-container{pointer-events:auto}"
        ;;Suggestion box is hidden otherwise.
        ".leaflet-container{z-index:0}"
        ;;kiwix-serve wraps an interface around the map. Force the position into the lower right corner.
        ".leaflet-control-attribution{position:fixed;bottom:0em;right:0em}"
        ".leaflet-control-scale{position:fixed;bottom:0em;left:0em}"
        ;;PruneCluster's default green cluster color visually vanishes.
        ".prunecluster-small{background-color:#71c6ff}"
        ".prunecluster-small div{background-color:#62adde}"
        ;;TODO: shadow per symbol.
        ".shadowed{background-color:rgba(235,235,235,0.7);box-shadow:0px 0px 10px #fff}"
        "html{height:100%;margin:0;padding:0;overflow:hidden}"
        "body{height:100%;margin:0;padding:0}"
        "#map{height:100%}")))

(defun map-file-leaflet-js (&key flavour locator? (url-base "./t/") (tile-size 256) (zoom-min 0) (zoom-max 20) (zoom-delta 2))
  "Common part of all maps. Less space is used in the ZIM file when the common part is linked instead of expanded.
   The query path stores multiple key value pairs. The key value pairs are separated by #\&. Key and value are separated by #\=.
   Supported keys:
    location: set a marker at #'locator-payload-encode -encoded location ABC.
    filter: only add layer features containing DEF.
    title: assign the title to document.title."
  (dir-entry
   :namespace "-"
   :url "map.js"
   :mimetype-id :js
   :content
   (dsl (list (format nil "//<![CDATA[~%") nil "//]]>")
        (dsl (list (format nil ";"))
             
             (defun-alphabetical-number-numerically)
             (defun-latlon-from-normalized)
             (defun-locator-payload-decode)
             "var input=new Object()"
             (dsl '("if(document.location.search!=''){" ";" "}")
                  ;;kiwix-android fails with this variant.
                  ;;"input=Object.fromEntries(window.location.search.substr(1).split('&').map(function (x){return x.split('=')}))"
                  "decodeURIComponent(document.location.search).substr(1).split('&').map(function (x){let kv=x.split('=');input[kv[0]]=kv[1]})")

             "var geom"
             (dsl '("if(input.location!==undefined){" ";" "}")
                  "geom=locatorPayloadDecode(input.location)")
             
             (dsl '("if(input.title!==undefined){" ";" "}")
                  "document.title=decodeURIComponent(input.title)")
             
             "var filter"
             (dsl '("if(input.filter!==undefined){" ";" "}")
                  "filter=decodeURIComponent(input.filter).replace('+',' ')"
                  "parent.window.document.title=document.title+' (Filter: '+filter+')'")
             
             (dsl ""
                  "var map="
                  (dsl "."
                       "L"
                       (jsq-funcall "map"
                                    "'map'"
                                    (dsl "{,}"
                                         "zoomAnimation:false"
                                         "fadeAnimation:false"
                                         "markerZoomAnimation:false"
                                         "animate:false"
                                         (dsl ":" "zoomSnap" zoom-delta)
                                         (dsl ":" "zoomSnapOrigin" (if (oddp zoom-max) 1 0))
                                         (dsl ":" "zoomDelta" zoom-delta)))))
             (dsl ""
                  "if(geom===undefined)"
                  (dsl "{;}" (format nil "map.fitBounds([[~A,~A],[~A,~A]],{animate:false})"
                                     (ll-lat (geometry flavour)) (ll-lon (geometry flavour))
                                     (ur-lat (geometry flavour)) (ur-lon (geometry flavour))))
                  "else if(geom.urLat===undefined)"
                  ;;ZOOM-MIN: extent is unknown.
                  (format nil (dsl "{;}" "map.setView([geom.llLat,geom.llLon],~A,{animate:false})") zoom-min)
                  "else"
                  ;;Input geometry is already splitted by the antimeridian.
                  (dsl "{;}" "map.fitBounds([[geom.llLat,geom.llLon],[geom.urLat,geom.urLon]],{animate:false})"))
             
             "var locatorLayer"
             (dsl '("if(geom!==undefined){" ";" "}")
                  (dsl "="
                       "let loadFurther"
                       ;;Interesting criss-cross of languages. → A renderer in JS would be nice.
                       (dsl "+"
                            ;;onclick: kiwix-android ignores window.location.search window.location.hash in links/forms, but does not interfere with JavaScript processing in the iframe.
                            (dsl :squote "<br><a href=\"#\" onclick=\"javascript:document.location.href=\\'../layer-selection.html?location=")
                            "input.location"
                            (dsl :squote "&title=")
                            "encodeURI(input.title)"
                            (dsl :squote "\\'\">")
                            (dsl :squote
                                 (if locator?
                                     "Additionally load one or multiple layers"
                                     "Choose new layers"))
                            (dsl :squote "</a>")))
                  (dsl "="
                       "locatorLayer"
                       (jsq-ternary
                        "geom.urLat===undefined"
                        (dsl "."
                             "L"
                             "marker([geom.llLat,geom.llLon])"
                             (jsq-funcall "bindPopup"
                                          (dsl "+"
                                               "'<b>'"
                                               "input.title"
                                               "'</b><br>'"
                                               "L.Geolet.formatLatLng({lat:geom.llLat,lon:geom.llLon})"
                                               "loadFurther")))
                        (dsl "."
                             "L"
                             (dsl '("polygon([" "," "])")
                                  "[geom.llLat,geom.llLon]"
                                  "[geom.urLat,geom.llLon]"
                                  "[geom.urLat,geom.urLon]"
                                  "[geom.llLat,geom.urLon]")
                             (jsq-funcall "bindPopup"
                                          (dsl "+"
                                               "'The minimum bounding rectangle for <b>'"
                                               "input.title"
                                               "'</b> is defined by:<ul><li>its SW corner at '"
                                               "L.Geolet.formatLatLng({lat:geom.llLat,lon:geom.llLon})"
                                               "'</li><li>and its NE corner at '"
                                               "L.Geolet.formatLatLng({lat:geom.urLat,lon:geom.urLon})"
                                               "'.</li></ul>'"
                                               "loadFurther")))))
                  "map.addLayer(locatorLayer)")
             
             (dsl "."
                  "L"
                  (jsq-funcall "tileLayer"
                               (format nil "'~A{z}/{x}/{y}.png'" url-base)
                               (dsl "{,}"
                                    (format nil "attribution:'Map data: &copy; <a href=\"../legal/index.html\" target=\"_parent\">OpenStreetMap contributors</a> | <a href=\"../legal/index.html\" target=\"_parent\">Legal notices</a>'")
                                    (dsl ":" "minZoom" zoom-min)
                                    "maxZoom:20"
                                    (dsl ":" "maxNativeZoom" zoom-max)
                                    (dsl ":" "zoomOffset" (zoom-offset tile-size))
                                    (dsl ":" "tileSize" tile-size)
                                    ;;Leaflet: "...every integer zoom level" → unusable for (< 1 zoomSnap): shows grey (even when pinching stopped).
                                    "updateWhenZooming:false"))
                  "addTo(map)")
             
             ;;../leaflet-~A/README.html
             (format nil "var originalPrefix=map.attributionControl.options.prefix.replace('https://leafletjs.com\"','../unresolutions.html\" target=\"_parent\"').replace('Leaflet','UN Resolutions')"
                     ;;(version :leaflet)
                     )
             "function describeCenter(){return 'Map center: '+L.Geolet.formatLatLng(map.getCenter())}"
             "map.attributionControl.setPrefix(describeCenter()+' | '+originalPrefix)"
             "map.on('moveend',function(){map.attributionControl.setPrefix(describeCenter()+' | '+originalPrefix)})"
             
             "L.control.scale().addTo(map)"
             "L.geolet({position:'topleft'}).addTo(map)"
             "var hash = L.hash(map)"
             
             "var overlays={}"
             (loop for i = 0 then (incf i)
                   for layer in (layers flavour)
                   collect (dsl
                            (list (format nil "if(~A!==undefined){" (name layer)) ";" "}")
                            ;;lv: leafletView
                            (format nil "var lv_~A=new PruneClusterForLeaflet()" (name layer))
                            ;;TODO: cross-site errors
                            #|"var request = new XMLHttpRequest();"
                            "request.open(\"GET\", \"../-/layers/hospitals.js\", false);"
                            "request.send(null)"|#
                            (dsl " "
                                 (format nil "for(var i=0,l=~A.length;i<l;++i)" (name layer))
                                 (dsl "{;}"
                                      (dsl ""
                                           (dsl '("if(" "" ")")
                                                (dsl '(" || ")
                                                     "filter===undefined"
                                                     (dsl '("(" " && " ")")
                                                          "filter!==undefined"
                                                          (format nil "~A[i][2].match(new RegExp(filter,'i'))" (name layer)))))
                                           (dsl "{;}"
                                                (jsq-funcall
                                                 (format nil "lv_~A.RegisterMarker" (name layer))
                                                 (dsl '("new PruneCluster.Marker(" nil ")")
                                                      (dsl ","
                                                           (format nil "~A[i][0]" (name layer))
                                                           (format nil "~A[i][1]" (name layer))
                                                           ;;Performance vs. memory utilization. → Feedback.
                                                           (dsl "{,}"
                                                                (dsl ":"
                                                                     "popup"
                                                                     (if (popup-name-renderer layer)
                                                                         (funcall (popup-name-renderer layer) (name layer))
                                                                         (format nil "~A[i][2]" (name layer))))
                                                                (dsl ":"
                                                                     "icon"
                                                                     (dsl '("L.icon({" "," "})")
                                                                          (format nil "iconUrl:'../../I/symbols/~A.svg'"
                                                                                  (name layer))
                                                                          "iconSize:[24,24]"
                                                                          "popupAnchor:[0,-18]"
                                                                          "className:'shadowed'"))))))))))
                            #|(dsl " "
                            (format nil "lv_~A.PrepareLeafletMarker = function (marker, data)" (name layer))
                            (dsl "{;}"
                            (dsl nil
                            "if (marker.getPopup())"
                            (dsl "{;}" "marker.setPopupContent(data.title)")
                            "else"
                            (dsl "{;}" "marker.bindPopup(data.title)")
                            (dsl "."
                            "marker"
                            (jsq-funcall "setIcon"
                            (dsl '("L.icon({" "," "})")
                            (format nil "iconUrl:'../symbols/~A.svg'" (name layer))
                            "iconSize:[48,48]"))))))|#
                            (dsl (list (format nil "if(~A!==undefined){" (name layer)) ";" "}")
                                 (format nil "Object.assign(overlays,{~A:lv_~A})"
                                         (dsl :quote (escape-js (layer-name-0nf layer flavour T 1)))
                                         (name layer)))
                            ;;If only one layer is selected, it is safe to assume that the users want to open the layer, since the "base map"/"multiple maps" would have been selected otherwise.
                            (dsl (list (format nil "if(selectedLayer!==undefined && selectedLayer==~A){" i) ";" "}")
                                 (format nil "map.addLayer(lv_~A)" (name layer)))))
             (dsl "="
                  "var markeroverlay"
                  (dsl "{:}"
                       (dsl "[]"
                            (jsq-ternary "(geom!==undefined && geom.urLat===undefined)"
                                         (dsl :quote (format nil "<img style='height:1em;margin-right:0.5em' src='../../-/leaflet-~A/images/marker-icon.png'> Show the point of interest"
                                                             (version :leaflet)))
                                         (dsl :quote "<div style='width:0.8em;height:0.8em;border:0.2em solid #3388ff;background-color:rgba(51,136,255,0.2);border-radius:0.3em;display:inline-block'>&nbsp;</div> Show the minimum bounding rectangle")))
                       "locatorLayer"))
             ;;TODO: Only load the files per JSON when the overlay is effectively loaded.
             #|(dsl " "
             "map.on"
             (dsl "(,)"
             (dsl :squote "overlayadd")
             (dsl " "
             "function(ev)"
             (dsl "{;}"
             ))))|#
             (dsl '("if(Object.keys(overlays).length>0 || locatorLayer){" "" "}")
                  (dsl "."
                       "L" "control"
                       (jsq-funcall "layers"
                                    "null"
                                    (jsq-ternary "locatorLayer"
                                                 "Object.assign(overlays,markeroverlay)"
                                                 "overlays"))
                       "addTo(map)"))))))

(defun map-file-leaflet (&key filename flavour (layer-selection 0) locator?)
  "An HTML page which loads a Leaflet-based map.
   LOCATOR?: whether the map is locator-map.html, i.e. in a frame. It should hide kiwix-serve's injected interface in this case. The interface of the place file will show up instead."
  (declare (ignorable locator?))
  (let* ((layers (layers flavour))
         (layer-selection (or layer-selection (ash 1 (length layers)))))
    (dir-entry
     :namespace "A"
     :url filename
     :mimetype-id :html
     :content
     (dsl nil
          "<!DOCTYPE html>"
          (dsl '("<html>" nil "</html>")
               (dsl '("<head>" nil "</head>")
                    "<meta charset=\"utf-8\">"
                    (dsl '("<title>" "</title>")
                         (if (= 0 layer-selection)
                             "Base map"
                             (dsl '(", ")
                                  (loop for i = 0 then (incf i)
                                        for layer in layers
                                        when (logbitp i layer-selection)
                                          collect (user-facing-name layer)))))
                    "<meta name=\"viewport\" content=\"width=device-width, maximum-scale=1.0, user-scalable=no\"/>"
                    ;;Docs say that leaflet.css should be loaded first.
                    (format nil "<link rel=\"stylesheet\" href=\"../../-/leaflet~@[-~A~]/leaflet.css\" />" (version :leaflet))
                    #|Wishlist:
                    - No animation when panning through keypresses (manually introduced).
                    - Respect map.options.zoomDelta when using scroll wheel (manually introduced).
                    - map.options.zoomSnap should also work for uneven maximal zooms (manually introduced).
                    - Zoom to the mouse cursor location instead to the map center when pressing the plus key.|#
                    (format nil "<script src=\"../../-/leaflet~@[-~A~]/leaflet_modified.js\"></script>" (version :leaflet))
                    (format nil "<script src=\"../../-/leaflet~@[-~A~]/leaflet-hash.js\"></script>" (version :leaflet))
                    (format nil "<link rel=\"stylesheet\" href=\"../../-/leaflet~@[-~A~]/PruneCluster~@[-~A~]/LeafletStyleSheet.css\" />" (version :leaflet) (version :prunecluster))
                    (format nil "<script src=\"../../-/leaflet~@[-~A~]/PruneCluster~@[-~A~]/PruneCluster.js\"></script>" (version :leaflet) (version :prunecluster))
                    (format nil "<script src=\"../../-/leaflet~@[-~A~]/GEOLET~@[-~A~]/geolet.js\"></script>" (version :leaflet) (version :geolet))
                    (dsl '("<script>" ";" "</script>")
                         (when layers (dsl '("var " "," nil) (loop for layer in layers collect (name layer))))
                         (if (= 1 (logcount layer-selection))
                             (format nil "var selectedLayer=~A" (round (log layer-selection 2)))
                             "var selectedLayer"))
                    (loop for i = 0 then (incf i)
                          for layer in layers
                          when (logbitp i layer-selection)
                            collect (format nil "<script src=\"../../-/layers/~A.js\"></script>" (name layer)))
                    "<link rel=\"stylesheet\" href=\"../../-/map.css\" />"
                    #|(when locator?
                      (dsl '("<style type=\"text/css\">" nil "</style>")
                           ".kiwix{display:none}"
                           "body{padding-top:0em !important}"))|#)
               (x-body (suppress-kiwix-interface)
                       "<div id=\"map\"></div>"
                       "<script src=\"../../-/map.js\"></script>"))))))

(defun skel-file (url my-mime relative-path &optional content (namespace "-"))
  "Quick helper for #'dir-entry."
  (dir-entry
   :namespace namespace
   :url url
   :mimetype-id my-mime
   :content content
   :relative-path relative-path))

(defun leaflet ()
  "Leaflet and its plugins (leaflet-hash Prunecluster GEOLET currently).
   Working directory: too/skel"
  (dir
   :name (format nil "leaflet-~A" (version :leaflet))
   :entries (list (skel-file "leaflet.css" :css (format nil "leaflet-~A/leaflet.css" (version :leaflet)))
                  (skel-file "leaflet_modifications.txt" :txt nil (format nil "leaflet.patch is the output of diff -w /upstream/leaflet_~A/leaflet-src.js /this-zim/leaflet-src.js" (version :leaflet)))
                  (skel-file "leaflet.patch" :txt (format nil "leaflet-~A/leaflet.patch" (version :leaflet)))
                  (skel-file "leaflet_modified.js" :js (format nil "leaflet-~A/leaflet-src.js" (version :leaflet)))
                  (skel-file "leaflet-hash.js" :js (format nil "leaflet-~A/leaflet-hash.js" (version :leaflet)))
                  (dir
                   :name "images"
                   :entries (list (skel-file "layers.png" :png (format nil "leaflet-~A/images/layers.png" (version :leaflet)))
                                  (skel-file "layers-2x.png" :png (format nil "leaflet-~A/images/layers-2x.png" (version :leaflet)))
                                  (skel-file "marker-icon.png" :png (format nil "leaflet-~A/images/marker-icon.png" (version :leaflet)))
                                  (skel-file "marker-icon-2x.png" :png (format nil "leaflet-~A/images/marker-icon-2x.png" (version :leaflet)))
                                  (skel-file "marker-shadow.png" :png (format nil "leaflet-~A/images/marker-shadow.png" (version :leaflet)))))
                  (dir
                   :name (format nil "PruneCluster-~A" (version :prunecluster))
                   :entries (list (skel-file "LeafletStyleSheet.css" :css (format nil "leaflet-~A/PruneCluster-~A/LeafletStyleSheet.css" (version :leaflet) (version :prunecluster)))
                                  (skel-file "PruneCluster.js" :js (format nil "leaflet-~A/PruneCluster-~A/PruneCluster.js" (version :leaflet) (version :prunecluster)))))
                  (dir
                   :name (format nil "GEOLET-~A" (version :geolet))
                   :entries (list (skel-file "geolet.js" :js (format nil "leaflet-~A/GEOLET-~A/geolet.js" (version :leaflet) (version :geolet)))))
                  (skel-file "logo.svg" :svg (format nil "leaflet-~A/logo.svg" (version :leaflet)) nil "I"))))

(defun too-source-code ()
  "A `dir instance with the source code of TOO.
   Working directory: too/skel/."
  ;;CACHE CONTROL: manually parsed primitive.
  (dir
   :name "too"
   :entries (list (skel-file "COPYING" :txt "../COPYING")
                  (skel-file "too.asd" :txt "../too.asd")
                  (skel-file "too-test.asd" :txt "../too-test.asd")
                  (skel-file "README.md" :txt "../README.md")
                  (dir
                   :name "docs"
                   :entries (list (skel-file "glossary.txt" :txt "../docs/glossary.txt")))
                  (dir
                   :name "example"
                   :entries (list (skel-file "setup.lisp" :txt "../example/setup.lisp")))
                  (dir
                   :name "legal"
                   :entries (list (skel-file "GEOLET.txt" :txt "../legal/GEOLET.txt")
                                  (skel-file "Leaflet.txt" :txt "../legal/Leaflet.txt")
                                  (skel-file "libapache2-mod-tile.txt" :txt "../legal/libapache2-mod-tile.txt")
                                  (skel-file "libosmium.txt" :txt "../legal/libosmium.txt")
                                  (skel-file "libzim.txt" :txt "../legal/libzim.txt")
                                  (skel-file "openstreetmap-carto.txt" :txt "../legal/openstreetmap-carto.txt")
                                  (skel-file "osm2pgsql.txt" :txt "../legal/osm2pgsql.txt")
                                  (skel-file "Parenscript.txt" :txt "../legal/Parenscript.txt")
                                  (skel-file "PruneCluster.txt" :txt "../legal/PruneCluster.txt")))
                  (dir
                   :name "skel"
                   :entries (list (leaflet)
                                  (skel-file "TOO-Logo.blend" :blend "TOO-Logo.blend")
                                  (skel-file "TOO-Logo-with-marker.png" :png "TOO-Logo-with-marker.png")
                                  (skel-file "TOO-Logo-without-marker.png" :png "TOO-Logo-without-marker.png")
                                  (dir
                                   :name "un"
                                   :entries (list (skel-file "A_RES_68_262-EN.pdf" :pdf "un/A_RES_68_262-EN.pdf")
                                                  (skel-file "A_RES_ES-11_1-EN.pdf" :pdf "un/A_RES_ES-11_1-EN.pdf")
                                                  (skel-file "A_RES_ES-11_2-EN.pdf" :pdf "un/A_RES_ES-11_2-EN.pdf")
                                                  (skel-file "A_RES_ES-11_3-EN.pdf" :pdf "un/A_RES_ES-11_3-EN.pdf")
                                                  (skel-file "A_RES_ES-11_4-EN.pdf" :pdf "un/A_RES_ES-11_4-EN.pdf")))
                                  (dir
                                   :name "legal"
                                   :entries (list (skel-file "odbl-10.txt" :txt "../skel/legal/odbl-10.txt")
                                                  (skel-file "AGPLv3.txt" :txt "../skel/legal/AGPLv3.txt")
                                                  (skel-file "CC0-1.0.txt" :txt "../skel/legal/CC0-1.0.txt")
                                                  (skel-file "CC-BY-SA-4.0.txt" :txt "../skel/legal/CC-BY-SA-4.0.txt")
                                                  (skel-file "BSD-2-Clause-Leaflet.txt" :txt "../skel/legal/BSD-2-Clause-Leaflet.txt")
                                                  (skel-file "MIT-leaflet-hash.txt" :txt "../skel/legal/MIT-leaflet-hash.txt")
                                                  (skel-file "MIT-GEOLET.txt" :txt "../skel/legal/MIT-GEOLET.txt")
                                                  (skel-file "MIT-PruneCluster.txt" :txt "../skel/legal/MIT-PruneCluster.txt")
                                                  #|
                                                  Copyright and contributors (COCO):
                                                  Linking to COCO doesn't make much sense in a ZIM file intended for offline use.
                                                  Now, it is possible to execute $(wget --adjust-extension --span-hosts --convert-links --page-requisites https://www.openstreetmap.org/copyright https://wiki.openstreetmap.org/wiki/Contributors), and the results are pretty nice overall in a local browser.
                                                  Problem is that both wget and zimwriterfs have problems with URLs. wget, for example, fails to rewrite the OSM logo in the srcset attribute (even though the logo gets downloaded because of the subsequent tag), whereas zimwriterfs struggles with fancy CSS filepaths that the wget command produces.
                                                  Those "missing" included external links are a red flag for zimcheck.
                                                  As a consequence, $(w3m -cols 99999999 -I utf8 -O utf8 Contributors.html > Contributors.txt) is the preferred solution for COCO currently.
                                                  |#
                                                  ;;CACHE CONTROL: cached w3m result.
                                                  (skel-file "Contributors.txt" :txt "../skel/legal/Contributors.txt")
                                                  ;;CACHE CONTROL: cached w3m result.
                                                  (skel-file "Copyright.txt" :txt "../skel/legal/Copyright.txt")))
                                  (skel-file "background_cropped_bright_navigable.png" :txt "../skel/background_cropped_bright_navigable.png")))
                  (dir
                   :name "src"
                   :entries (list (skel-file "package.lisp" :txt "../src/package.lisp")
                                  (skel-file "common.lisp" :txt "../src/common.lisp")
                                  (skel-file "lisp-at-work.lisp" :txt "../src/lisp-at-work.lisp")
                                  (skel-file "uncharter.lisp" :txt "../src/uncharter.lisp")
                                  (skel-file "osm2pgsql-indexing.lisp" :txt "../src/osm2pgsql-indexing.lisp")
                                  (skel-file "zim-general.lisp" :txt "../src/zim-general.lisp")
                                  (skel-file "map-tiles.lisp" :txt "../src/map-tiles.lisp")
                                  (skel-file "zim-with-tiles.lisp" :txt "../src/zim-with-tiles.lisp")
                                  (skel-file "unresolutions.lisp" :txt "../src/unresolutions.lisp")
                                  (skel-file "opfff.lisp" :txt "../src/opfff.lisp")
                                  (skel-file "wiki-definition.lisp" :txt "../src/wiki-definition.lisp")
                                  (skel-file "layer.lisp" :txt "../src/layer.lisp")
                                  (skel-file "release-related.lisp" :txt "../src/release-related.lisp")
                                  (skel-file "geocode.lisp" :txt "../src/geocode.lisp")
                                  (skel-file "user-interface.lisp" :txt "../src/user-interface.lisp")))
                  (dir
                   :name "test"
                   :entries (list (skel-file "package.lisp" :txt "../test/package.lisp"))))))

(defun zim-contents (&key (standalone? T) (flavour (first *flavours*)) (force-local-server? T))
  "Defines the files fitted to a FLAVOUR.
   STANDALONE? nil: do not build tile cache (localhost tile URL).
   Prototype: FLAVOUR whose region has the nickname \"prototype\", STANDALONE? is set to nil.
   Destructively modifies (i.e. overrides) the file /dev/shm/too.zip."
  (flet ((wrap-file-in-html (relative-path url-without-html title)
           "Read the contents of the file in RELATIVE-PATH, wrap them in HTML and set the resulting string as the content of a new `dir-entry."
           (dir-entry
            :url (dsl "" url-without-html ".html")
            :title title
            :mimetype-id :html
            :content (x-html (x-head "<meta charset=utf8>" (x-title title))
                             (x-body (x-pre
                                      :style "white-space:pre-wrap"
                                      (escape-xml (uiop:read-file-string
                                                   (absolute-path (make-instance 'dir-entry-file
                                                                                 :relative-path relative-path))))))))))
    (let* ((zoom-delta 2)
           (loc-files (loc-files flavour))
           ;;DEPENDENCY: Feeds *layer-entries-count*, which is read by #'index-file. → Moved above `dir constructor call.
           (cached-layers (loop for layer in (layers flavour)
                                collect (dir-entry
                                         :namespace "-"
                                         :url (format nil "~A.js" (name layer))
                                         :mimetype-id :js
                                         :relative-path (create-clusterable-layer
                                                         :layer layer
                                                         :flavour flavour)))))
      ;;The detour via /dev/shm/too.zip is a KLUDGE (that works).
      (zip:with-output-to-zipfile (zip "/dev/shm/too.zip" :if-exists :supersede)
        (loop for entry in (flatten-dir (if (string-equal "prototype" (nickname flavour))
                                            (dir :name "too"
                                                 :entries (list (skel-file "README.md" :txt nil
                                                                           "The prototype is only intended as a decision help for the OpenStreetMap Foundation. Please visit https://library.kiwix.org/ (category \"OpenStreetMap\") for actual releases that include TOO's source code, too.")))
                                            (too-source-code)))
              do (cond ((typep entry 'dir-entry-file)
                        (with-open-file (f (absolute-path entry) :element-type '(unsigned-byte 8))
                          (zip:write-zipentry zip (url entry) f :file-write-date nil)))
                       ((typep entry 'dir-entry-memory)
                        (zip:write-zipentry zip (url entry)
                                            (flexi-streams:make-in-memory-input-stream
                                             (flexi-streams:string-to-octets (content entry) :external-format *flexi-utf8*))
                                            :file-write-date nil)))))
      (dir
       :name "too"
       :entries
       (asplice
        ;;Release part of file name.
        (metaq "Name" (format nil "too-~A" (final-nickname flavour)))
        (metaq "Title" "The Offline Oriented")
        (metaq "Creator" "Map: OpenStreetMap contributors, rest: TOO project members")
        ;;TODO: contact Kiwix.
        (metaq "Publisher" "Kiwix")
        ;;Intentionally leaning towards the publication date, and ignoring the ZIM file creation date. TOO dates should be understood as "map dates".
        (metaq "Date" (format nil "~A-~2,'0D-~2,'0D" (year flavour) (month flavour) (day flavour)))
        (metaq "Published" (format nil "~A-~2,'0D-~2,'0D" (year flavour) (month flavour) (day flavour)))
        ;;"A rendered map with layers"
        (metaq "Description" (format nil "~A, z~2,'0D~@[-~A~] (~A-~2,'0D)"
                                     (string-upcase (user-facing-name flavour) :start 0 :end 1)
                                     (zoom-max flavour)
                                     (when (gethash (zoom-max flavour) *zoom-levels*)
                                       (zoom-nickname (zoom-max flavour)))
                                     (year flavour) (month flavour)))
        (metaq "LongDescription" "The TOO project releases maps for offline use. Releases are made in a semi-regular interval for different regions. Since the TOO project is relatively young, the only region released so far is 'earth' (global coverage). TOO, the software of the TOO project, is free&libre software (AGPLv3+) and copyright 2022 Benedikt Steger, whereas the data source, the OpenStreetMap™ is copyright OpenStreetMap contributors and released under the Open Database License 1.0.")
        ;;Welcome file etc. Geocoding actually includes name:en, too.
        (metaq "Language" "eng")
        (metaq "License" "Map data: ODbL 1.0, TOO: AGPLv3+, see article 'Legal notices' for an exact breakdown.")
        (metaq "Tags" "maps;_category:maps;_pictures:yes;_videos:no;_details:yes;_ftindex:no")
        ;;Only maxzoom, no release part and and no date.
        (metaq "Flavour" (format nil "z~2,'0D~@[-~A~]"
                                 (zoom-max flavour)
                                 (when (gethash (zoom-max flavour) *zoom-levels*)
                                   (zoom-nickname (zoom-max flavour)))))
        (metaq "Scraper" (format nil "TOO ~A" (version :too)))
        (metaq "Source" "https://www.openstreetmap.org/")
        (metaq "Illustration_48x48@1" *empty-favicon* :png "TOO-Logo-with-marker.png")
        (dir-entry :namespace "-" :url "favicon" :target-url "M/Illustration_48x48@1")
        ;;(dir-entry :namespace "-" :url "favicon" :title "favicon" :content *empty-favicon* :mimetype-id (mimeq :png) :relative-path "./osmlogo48x48.png")
        
        (dir-entry :namespace "I" :url "background_cropped_bright_navigable.png" :mimetype-id :png :relative-path "./background_cropped_bright_navigable.png")
        ;;TODO: kiwix-android obviously does not follow redirects of images or something is wrong with the way TOO is creating the ZIM file. Now, since the redirections work both in kiwix-desktop and in kiwix-serve, I decide to keep the deduplication redirects in general. Only this file is intentionally duplicated and the empty files in the kiwix-android version won't show up in the map. This is ok as they are, well, empty.
        ;;(dir-entry :namespace "I" :url "TOO-Logo-with-marker.png" :target-url "M/Illustration_48x48@1")
        (dir-entry :namespace "I" :url "TOO-Logo-without-marker.png" :mimetype-id :png :relative-path "./TOO-Logo-without-marker.png")
        (dir-entry :namespace "I" :url "TOO-Logo-with-marker.png" :mimetype-id :png :relative-path "./TOO-Logo-with-marker.png")
        (dir
         :name "layers"
         :entries cached-layers)
        (index-file
         :flavour flavour
         :num-loc-files (length loc-files))
        (dir-entry :namespace "W" :url "mainPage" :target-url "A/index.html")
        (dir-entry :url "too.zip"
                   :title "TOO's source code"
                   :mimetype-id :zip
                   :relative-path "/dev/shm/too.zip")
        ;;The workaround around kiwix-android's stateless links extends to name-lookup.html, the target for the UN resolutions link in the map. _parent links don't work with anchors. Consequently, the map must link to a special file which resolves an anchored link itself.
        (dir-entry :url "unresolutions.html"
                   :title "UN resolutions"
                   :mimetype-id :html
                   :content (x-html (x-head "<meta http-equiv='Refresh' content='0; url=name-lookup.html#ukraine' />")))
        (dir :name "un"
             :entries (cons (uncharter)
                            (append (a-res-68-262)
                                    (a-res-es-11-1)
                                    (a-res-es-11-2)
                                    (a-res-es-11-3)
                                    (a-res-es-11-4))))
        (dir
         :name "legal"
         :entries (list (copyright-file)
                        (wrap-file-in-html "legal/Contributors.txt" "Contributors" "Contributors")
                        (wrap-file-in-html "legal/Copyright.txt" "Copyright" "Copyright")
                        (wrap-file-in-html "legal/odbl-10.txt" "odbl-10" "Open Database License 1.0" )
                        (wrap-file-in-html "legal/CC-BY-SA-4.0.txt" "CC-BY-SA-4.0" "Creative Commons - Attribution - Sharealike version 4.0")
                        (wrap-file-in-html "legal/CC0-1.0.txt" "CC0-1.0" "CC0 Public Domain Dedication, version 1.0")
                        (wrap-file-in-html "legal/BSD-2-Clause-Leaflet.txt" "BSD-2-Clause-Leaflet" "Leaflet's BSD 2-clause License")
                        (wrap-file-in-html "legal/MIT-leaflet-hash.txt" "MIT-leaflet-hash" "leaflet-hash's MIT license")
                        (wrap-file-in-html "legal/MIT-PruneCluster.txt" "MIT-PruneCluster" "PruneCluster's MIT license")
                        (wrap-file-in-html "legal/MIT-GEOLET.txt" "MIT-GEOLET" "GEOLET's MIT license")
                        (wrap-file-in-html "legal/AGPLv3.txt" "AGPLv3" "GNU Affero General Public License 3")))
        (name-lookup-file flavour)
        (map-file-leaflet-css)
        (map-file-leaflet-js
         :tile-size (tile-size flavour) :zoom-min (zoom-min flavour) :zoom-max (zoom-max flavour) :zoom-delta zoom-delta
         :url-base (cond (standalone?
                          ;;#\t means tiles.
                          "../../I/t/")
                         ((and (not force-local-server?)
                               (or
                                ;;Prototype for the OSMF (sent 20221014, sha1sum 31253b9e0dc1c0c8d56ae59c5378c0cc546498e1).
                                (string-equal "prototype" (nickname flavour))
                                ;;Demo file for UN's OSS4SDG (target publication date: 20221030).
                                (string-equal "nyc" (nickname flavour))))
                          "https://tile.openstreetmap.org/")
                         (T (format nil "http://127.0.0.1/osm~A/" (tile-size flavour))))
         :flavour flavour)
        (dir
         :name "l"
         :entries (cons (map-file-leaflet
                         :filename "locator-map.html"
                         :flavour flavour
                         :locator? T)
                        loc-files))
        (dir
         :name "symbols"
         :entries (loop for layer in (layers flavour)
                        collect (dir-entry
                                 :namespace "I"
                                 :url (format nil "~A.svg" (name layer))
                                 :mimetype-id :svg
                                 :content (map-symbol layer))))
        (dir
         :name "wiki"
         :entries (loop for article being the hash-value of *wiki-definitions*
                        when (find (name article)
                                   (asplice "City"
                                            "Town"
                                            "Village"
                                            "Hamlet"
                                            "Suburb"
                                            "Quarter"
                                            "Neighbourhood"
                                           (loop for layer in (layers flavour)
                                                 append (wiki-definitions layer)))
                                   :test #'string=)
                          collect (wiki-article-file article)))
        (layer-selection-file flavour)
        (dir
         :name "layer-selection"
         :entries (let ((combinations (ash 1 (length (layers flavour)))))
                    (loop for combination from 0 below combinations
                          collect (map-file-leaflet
                                   :filename (format nil (format nil "~~~A,'0B.html" (1- (integer-length combinations))) combination)
                                   :flavour flavour
                                   :layer-selection combination))))
        #|(dir
        :name "t"
        :entries (list (skel-file "tile-urls" :txt (when standalone?
        (wget-tiles-in-mbr
        :mbr (geometry flavour)
        :zoom-min (as-non-256px-zoom (zoom-min flavour) (tile-size flavour))
        :zoom-max (as-non-256px-zoom (zoom-max flavour) (tile-size flavour))
        :zoom-delta zoom-delta
        :url-base (format nil "http://127.0.0.1/osm~A/" (tile-size flavour)))))))|#
        (loop for i = 0 then (incf i)
              for identifying-part in *identifying-parts-of-empty-tiles*
              collect (dir-entry
                       :namespace "I"
                       :url (format nil "e~A.png" i)
                       :mimetype-id :png
                       :content (empty-tile identifying-part)))
        (leaflet)
        (osm-scales-file))))))

(defun flavour2zimfile (&key (flavour (first *flavours*)) (output-dir "/dev/shm/") (metatile-dir-base "/var/lib/mod_tile/") (standalone? T) (force-local-server? T) (compression-level 22))
  "TOO's entry function which writes a rendered FLAVOUR to a ZIM file.
    Supersedes any file with a matching #'zim-file-name.
   FLAVOUR: an instance of `flavour, typically one from *flavours*.
    Also see #'register-release-flavours.
   OUTPUT-DIR: the directory the ZIM file is created in, with a trailing slash.
   STANDALONE?: Whether map tiles are included. If not, the map files use the URL http://127.0.0.1/osm1024/{z}/{x}/{y}.png.
   FORCE-LOCAL-SERVER?: The prototype is special in that it refers to the official tile servers. Setting FORCE-LOCAL-SERVER? to T overrides this behaviour."
  (contents2zimfile :zim-contents (zim-contents
                                   :flavour flavour
                                   :standalone? standalone?
                                   :force-local-server? force-local-server?)
                    :flavour flavour
                    :out-file (cl-fad:merge-pathnames-as-file
                               output-dir
                               (zim-file-name (final-nickname flavour) (zoom-max flavour) (year flavour) (month flavour)))
                    :metatile-dir-base metatile-dir-base
                    :compression-level compression-level
                    :standalone? standalone?))

