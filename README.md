

# Export org-mode file to SKOS rdf files

`ox-skos.el` is an [Emacs Lisp](https://en.wikipedia.org/wiki/Emacs_Lisp) library to help exporting [org-mode](http://orgmode.org/) files
into [SKOS](https://en.wikipedia.org/wiki/Simple_Knowledge_Organization_System) files.

See the `examples/` directory for introductory examples.

Your org-mode file needs to use specific header properties and entry
properties, as shown below.


## Header properties

    #+CONTRIBUTOR: Bastien Guerry
    #+TYPE: Thesaurus
    #+DESCRIPTION: Vocabulary
    #+PUBLISHER: Bastien Guerry
    #+HOMEPAGE: https://data.culture.fr/thesaurus/
    #+RIGHTS: CC-by-sa-3.0
    #+LANGUAGE: fr
    #+conceptschemebaseuri: https://bzg.fr/thesaurus/resource/ark:/111111
    #+conceptschemeid: T111


## Entry properties

A SKOS concept needs to be entered with special properties:

    * Concept_one
      :PROPERTIES:
      :skos:definition: :fr Une définition pour concept_one.
      :skos:definition: :en A definition for concept_one.
      :skos:preflabel: :fr ConceptOne
      :skos:preflabel: :en ConceptOne
      :uri:      concept one
      :ID:       1
      :END:
    
    Une définition pour concept_one.


## Install and basic usage

Load ox-skos.el as you would load any other Emacs lisp library.

Then use `C-c C-e s s` to export the current org-mode buffer to a SKOS
buffer, or `C-c C-e s S` to export the current org-mode buffer to a SKOS
file.


## Compatibility

`ox-skos.el` requires Emacs >24 and org-mode >9.


## License

`ox-skos.el` is licensed under the GNU GPLv3 license or any later
version of it.

