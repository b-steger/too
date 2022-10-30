# The Offline Oriented (TOO)

## About

TOO (The Offline Oriented) creates offline available map files. The output files adhere to the ZIM format and can therefore be opened by ZIM readers such as the Kiwix browser. The generated ZIM files include Leaflet for map display, cache prerendered map tiles, store a selection of layers that display 500k features reasonably fast through clustering, and contain a disambiguated place name index which resolves place names to 1. points and 2. MBRs for non-point sources.

There are four possibly noteworthy technical solutions in TOO. First of all, TOO is able to reversibly pack multiple variable-length numbers of any size into a single number in a self-describing fashion (#'snip-number-encode). Secondly, TOO is self-hosted in the sense that it packages itself into the produced ZIM files. Thirdly, parts of the language-foreign libraries libzim and libapache2-mod-tile are implemented according to TOO's requirements by TOO itself, i.e. TOO does not depend on them, even though TOO reads from metatiles and even though TOO writes ZIM files.

The last technical solution that is presented here evolved out of TOO's license, so to speak. Unlike lisp-at-work, TOO's content output functions operate largely on strings, i.e. rendered representations of foreign language primitives. While this renders a comprehensive escaping solution impossible, an interesting and interoperable #'dsl method suited for generally applicable drafts and gradual structure accumulation came into existence. Always being able to precisely and steplessly adjust the concept representation to its complexity aligns remarkably well with the exploratory programming spirit of Common Lisp - I even introduced a pattern-matchable #'dsl variant in lisp-at-work as a consequence.

As a matter of fact, you will notice that TOO simply is a continued technical extension of the descriptions that you'll find both in the code and in the ZIM files. Writing actual sentences automatically sorts the concepts in the head and informs everything the whole time - from the architectural design down to the way simple functions are implemented. That's also the reason why there is an accompanying docs/glossary.txt that explains how certain terms are used within the code and within the comments.

Code readers are advised to read the pages in a TOO ZIM file before continuing with the inductive approach of reading the source files in the order listed in the too.asd file.

example/setup.lisp contains instructions for getting started.


## License

TOO (The Offline Oriented) creates ZIM files with offline available maps
Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.


TOO contains adapted code snippets from other libraries. The folder ```legal/``` lists the copyright remarks of those libraries.

For the license terms of referenced Common Lisp libraries consult the respective files in the Quicklisp packages.

Also see the LEGAL NOTICES in a TOO ZIM file (≙ #'copyright-file in src/user-interface.lisp).

