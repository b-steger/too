;;;; TOO (The Offline Oriented) creates ZIM files with offline available maps
;;;; Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>
;;;; 
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-user)

(defpackage #:too-test
  (:use #:cl))

(in-package :too-test)

#|
Testing is done by:
1. Writing calling forms below the function definitions. Those forms are called with M-x or inspected with C-I. They grow in an evolutive fashion and may target edge cases (see, for example, too::*zim-example*).
2. Testing the ZIM file with kiwix-desktop.
    Reset caches by deleting the development ZIM file from the list of ZIM files (affects fulltext searches only).
3. Starting a server from within kiwix-desktop (a kiwix-serve would be identical).
    Attention: the browser may cache pages.
4. Downloading the ZIM file to an Android tablet and opening the ZIM file there with kiwix-android.
5. zimcheck / zimcheck --empty -CUIMFPUXED *.zim
    Files with incomplete tile caches pass the test, though.
6. Asking for feedback.

Test cases:
1. Quickload TOO without errors in a fresh Lisp image.
2. Replay the forms in example/setup.lisp without errors.
3. Proof-read texts and check for wrongly communicated program behaviour.
4. Open the base map, pan around and zoom as far as possible into two places (and away from them).
   Expected: "Base map" title, zoom control, geolocator, scale bar, updated map center, attribution, snapped zoom levels.
5. Start a local Kiwix server and check the base map in the browser.
6. Load a layer. Find a popup with multiple attributes. Check for correct symbols next to the checkboxes in the menu in the upper right corner.
7. Load a layer selection file with two thematically related layers. Open the popups. Check that the features do not share locations.
8. Load the selection file with all layers. Open the popups. Validate whether the cached counters are plausible.
9. Check the Leaflet/attribution link, license links, the links back to the main page, the link to the name lookup page, the links to TOO.zip.
    HTML files should include the background.
    (zimcheck)
10. Search for a capital and open an entry. Expected: a point marker. Open the popup.
11. Search for an administrative district you know and open an entry. Expected: a MBR.
12. Search for a village with name collisions. Open both entries.
13. Search for a country, and choose a name with non-latin characters. Search for a country with multiple geometries, open the first three. Open the popup.
14. In the popup of a country, choose additional layers. Select two layers and "open the faster map". The search result should still be selectable.
15. Continue test case 14 with another layer selection.
16. Build a standalone ZIM file for the minimal zoom level (in /dev/shm).
17. Verify that the SQL queries have indices (+ that they are registered in src/osm2pgsql-indexing.lisp).
18. Build a standalone file for the maximal zoom level (possibly non-/dev/shm).
19. Build a flavour with a non-earth-sized region.
     Expected: search index and layers are limited to region intersections. (Meta-)tiles are global up to zoom 6, intersecting the region otherwise.
20. Look at some licenses, check out the contributor list and download TOO's source code.
21. In "Name lookup", search for a feature with a non-latin character and a space.
22. Create a flavour, stop the Lisp image, ensure that /dev/shm still has the cached layers, start a fresh Lisp image, create the same flavour and verify that the number of features are listed correctly.
FUTURE (depends on feedback actually)
23. Check whether tiles/layers/search index are available for all MBRs of the region.
24. Check whether the OPFFF file with holes filters correctly.
25. Measure battery life of a portable device with a vector map and compare to a ZIM file with a raster map.
|#

