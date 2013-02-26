* too many errors on generation
* webglue uris without ID => list all uris in MAP_PATH, or NOT.
* --recurse is really not efficient since we basically generate
  everything twice. 
* cleanup ccontent. 
* remove the notion of w.id, it is unclear and not really necessary.
* introduce (with-subst eatom e bla bla bla) ? Use case : markup / non 
  markup 
* Review load procedure to maximize includes sharing
* w.includes should never include w.format this gives weird results
* content -r, error reported twice because of content and then deps  

* grep for w.get-opt 
* http://www.data-vocabulary.org/Person/

* Uris
** Changing the URI does not rebuild.
** Check URI unicity (?) ==> diagnose
** Bugs "http://" URI is considered relative.

* General 
** Document and do something about errors in `derived data. 
** also check about deps unfound errors 
** Build systems are never good at erasing stuff.
   Also w.opt-get, todo how to handle dep addition, e.gl w.self had no title
   and suddenly has one.
   (ids news works)
   (ids-titles (w.list 
      (values (w.get ids) w.self)
      (item (w.opt-get w.item title))))

* Documentation 
** Clarify in Format.T.create that the map is already in the db an then 
   replaced by the resulting values. 
** Standard_uris indicate which functions are implemented. 

* Webglue tool
** webglue find -p
** webglue-diagnose. without id, go through all map files 
   specified in map-path, 
** Site generation, Introduce a way to bring in orphans => sitemap.xml

* Formats
** Review w.files, and introduce (include-all) <-- Doesn't make sense.
** Check errors in w_text (w_files ?) when files are missing.
** Check reserved key error. 
** for switches don't use presence/absence, use boolean 
   and a default. 

* XML generation
** Normalize UTF-8 to NFC, see http://www.w3.org/TR/charmod-norm/.
** http://qa-dev.w3.org/i18n-checker/
** (doc-pretty) would be nice. 
** XHTML vs HTML in w.xml format
*** Escape lone & (e.g. profile) and introduce notation for entities.
    DONE, TODO document. 
*** Problem with self-closing tags. <div /> allowed in xhtml but not in html.
*** Implement html markup
*** Pretty output 

* Examples
** Redo examples in locale-menus with lang and hreflang 

* w.sitemap
** sm-lastmod try to use stat info, 
   or maybe not, since in fact if a dep change the map's stat will
   not change even though its final content will. 
** Devise a better strategy for modification times. 
   stat doesn't work when build system because of cp. 


* myocamlbuild.ml
** why without <**/*.ml> : map_dir (maps) it doesn't work ? 

* Formats
** Format to compute css scales. 


