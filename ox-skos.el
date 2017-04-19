;;; ox-skos.el --- SKOS Back-End for Org Export Engine

;; Copyright (C) 2017 Bastien Guerry

;; Author: Bastien Guerry <bzg@gnu.org>
;; Keywords: skos

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a SKOS back-end (as TTL or XML/RDF) for Org
;; exporter, based on the `html' back-end.
;;
;; This backend understands these new option keywords:
;;
;; #+SKOS_EXTENSION: rdf (the default)
;; #+SKOS_CONCEPTSCHEMEURI: set the base concept scheme URI.
;;
;;; Todo:
;;
;; - use :skos:note:fr: to specify the language
;; - use :skos:note:1 or :skos:note:note_label for multiple notes
;; - implement related (add var with list of properties?)
;; - use SKOS_EXTENSION for ttl output ?
;; - write ox-skos-html to export to html
;; - write ox-skos-latex to export to latex (anf scribble?!)

;;; Code:

(require 'ox-html)
(require 'ob-core)
(require 'url-util)
(declare-function url-encode-url "url-util" (url))
(declare-function org-babel-parse-header-arguments "ob-core" (arg-string))

;;; Variables and options

(defgroup org-export-skos nil
  "Options specific to SKOS export back-end."
  :tag "Org SKOS"
  :group 'org-export)

(defcustom org-skos-extension "rdf"
  "File extension for the SKOS output file."
  :group 'org-export-skos
  :type 'string)

;;; Define backend

(org-export-define-derived-backend 'skos 'html
  :menu-entry
  '(?s "Export to SKOS"
       ((?s "As SKOS buffer"
	    (lambda (a s v b) (org-skos-export-as-skos a s v)))
	(?S "As SKOS file" (lambda (a s v b) (org-skos-export-to-skos a s v)))))
  :options-alist
  '((:description "DESCRIPTION" nil nil newline)
    (:coverage    "COVERAGE" nil nil newline)
    (:type        "TYPE" nil nil newline)
    (:source      "SOURCE" nil nil newline)
    (:contributor "CONTRIBUTOR" nil nil newline)
    (:homepage    "HOMEPAGE" nil nil newline)
    (:rights      "RIGHTS" nil nil newline)
    (:publisher   "PUBLISHER" nil nil newline)
    (:subject     "SUBJECT" nil nil newline)
    (:keywords    "KEYWORDS" nil nil space)
    (:skos-conceptschemeuri "SKOS_CONCEPTSCHEMEURI" nil nil t)
    (:with-toc nil nil nil)
    (:skos-extension "SKOS_EXTENSION" nil org-skos-extension))
  :filters-alist '((:filter-final-output . org-skos-final-function))
  :translate-alist '((headline . org-skos-headline)
		     (comment . (lambda (&rest args) ""))
		     (comment-block . (lambda (&rest args) ""))
		     (section . org-skos-section)
		     (paragraph . (lambda (&rest args) ""))
		     (template . org-skos-template)))

;;; Export functions

;;;###autoload
(defun org-skos-export-as-skos (&optional async subtreep visible-only)
  "Export current buffer to a SKOS buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org SKOS Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'skos "*Org SKOS Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-skos-export-to-skos (&optional async subtreep visible-only)
  "Export current buffer to a SKOS file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name
		  (concat "." org-skos-extension) subtreep)))
    (org-export-to-file 'skos outfile async subtreep visible-only)))

;;;###autoload
(defun org-skos-publish-to-skos (plist filename pub-dir)
  "Publish an org file to SKOS.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to
   'skos filename (concat "." org-skos-extension) plist pub-dir))

;;; Main transcoding functions

(defun org-skos-i18n (value lang attr)
  "Convert VALUE with LANG into xml attribute ATTR.
VALUE can be a string or an alist."
  (if (null value) ""
    (let ((values (org-babel-parse-header-arguments value)))
      (mapconcat
       (lambda (lv)
	 (let ((l (if (cdr lv) (substring (symbol-name (car lv)) 1) lang))
	       (v (if (cdr lv) (cdr lv) (symbol-name (car lv)))))
	   (format "<%s xml:lang=\"%s\">%s</%s>" attr l v attr)))
       values
       "\n"))))

(defun org-skos-headline (headline contents info)
  "Transcode HEADLINE element into SKOS format.
CONTENTS is the headline contents.  INFO is a plist used as a
communication channel."
  (let* ((uri (concat "#" 
		      (or (org-element-property :ID headline)
			  (url-encode-url
			   (org-element-property :URI headline)))))
	 (lang (org-export-data (plist-get info :language) info))
	 (timestr (format-time-string-ISO-8601))
	 (notation
	  (org-skos-i18n
	   (org-element-property :SKOS:NOTATION headline) lang "skos:notation"))
	 (example
	  (org-skos-i18n
	   (org-element-property :SKOS:EXAMPLE headline) lang "skos:example"))
	 (note
	  (org-skos-i18n
	   (org-element-property :SKOS:NOTE headline) lang "skos:note"))
	 (altlabel
	  (org-skos-i18n
	   (org-element-property :SKOS:ALTLABEL headline) lang "skos:altLabel"))
	 (altlabel
	  (org-skos-i18n
	   (org-element-property :SKOS:ALTLABEL headline) lang "skos:altLabel"))
	 (preflabel
	  (org-skos-i18n
	   (or (org-element-property :SKOS:PREFLABEL headline)
	       (org-element-property :raw-value headline))
	   lang "skos:prefLabel"))
	 (conceptschemeuri
	  (url-encode-url (plist-get info :skos-conceptschemeuri)))
	 (broader
	  (org-element-property :URI (org-export-get-parent-headline headline)))
	 (narrower  ;; a list of narrower URIs
	  (org-element-map (plist-get info :parse-tree) 'headline
	    (lambda (h)
	      (if (equal
		   (org-element-property
		    :raw-value (org-export-get-parent-headline h))
		   (org-element-property :raw-value headline))
		  (org-element-property :URI h)))))
	 (parent (org-element-property :parent headline))
	 ;; FIXME use org-export-get-previous-element?
	 (first-para
	  (car (delete
		nil
		(org-element-map parent 'paragraph
		  (lambda (p)
		    (if (eq (org-element-property
			     :parent (org-element-property :parent p))
			    headline)
			p))))))
	 (definition
	   (or (org-skos-i18n
		(org-element-property :SKOS:DEFINITION headline)
		lang "skos:definition")
	       (and first-para
		    (format "<skos:definition xml:lang=\"%s\">%s</skos:definition>"
			    lang
			    (org-trim
			     (buffer-substring-no-properties
			      (org-element-property :contents-begin first-para)
			      (org-element-property :contents-end first-para)))))
	       "NO DEFINITION")))
    (concat
     ;; Add basic SKOS info
     (format
      "<skos:Concept rdf:about=\"%s%s\">
  <rdf:type rdf:resource=\"http://www.w3.org/2004/02/skos/core#Concept\"/>
  <skos:inScheme>
    <skos:ConceptScheme rdf:about=\"%s\"/>
  </skos:inScheme>
<dct:modified>%s</dct:modified>
<dct:created>%s</dct:created>
  %s
  %s
  %s
  %s
  %s
  %s
<iso-thes:status>1</iso-thes:status>
"
      timestr timestr
      conceptschemeuri uri conceptschemeuri
      definition notation preflabel altlabel example note)
     ;; Possibly add "broader"
     (if broader
  	 (format "<skos:broader rdf:resource=\"%s%s\"/>\n" conceptschemeuri broader))
     ;; Possibly add "narrower"
     (if narrower
	 (mapconcat
	  (lambda (n)
	    (format "<skos:narrower rdf:resource=\"%s%s\"/>" conceptschemeuri n))
	  narrower "\n"))
     ;; Possibly add topConceptOf
     (if (= (org-element-property :level headline) 1)
  	 (format "<skos:topConceptOf rdf:resource=\"%s\"/>" conceptschemeuri))
     "\n</skos:Concept>\n"
     contents)))

(defun format-time-string-ISO-8601 ()
  (concat
   (format-time-string "%Y-%m-%dT%T")
   ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
    (format-time-string "%z"))))

(defun org-skos-build-top-level-description (contents info)
  (let ((conceptschemeuri (plist-get info :skos-conceptschemeuri))
	(description (plist-get info :description))
	(lang (org-export-data (plist-get info :language) info))
	(title (org-export-data (plist-get info :title) info))
	(publisher (org-export-data (plist-get info :publisher) info))
	(homepage (org-export-data (plist-get info :homepage) info))
	(rights (org-export-data (plist-get info :rights) info))
	(email (org-export-data (plist-get info :email) info))
	(type (org-export-data (plist-get info :type) info))
	(source (org-export-data (plist-get info :source) info))
	(author (org-export-data (plist-get info :author) info))
	(contributor (org-export-data (plist-get info :contributor) info))
	(subject (org-export-data (plist-get info :subject) info))
	(coverage (org-export-data (plist-get info :coverage) info))
	(timestr (format-time-string-ISO-8601)))
    (concat
     (format "<skos:ConceptScheme rdf:about=\"%s\">
<rdf:type rdf:resource=\"http://www.w3.org/2004/02/skos/core#ConceptScheme\"/>
<dc:rights>%s</dc:rights>
<dct:created>%s</dct:created>
<dc:relation></dc:relation>
<dct:issued>Publication</dct:issued>
<dct:modified>%s</dct:modified>
<dc:type>%s</dc:type>
<dc:source>%s</dc:source>
<dc:subject>%s</dc:subject>
<dc:coverage>%s</dc:coverage>
<dc:language>%s-%s</dc:language>
<dc:publisher>%s</dc:publisher>
<dc:contributor>%s</dc:contributor>
<dc:creator>
  <foaf:Organization>
    <foaf:mbox>%s</foaf:mbox>
    <foaf:homepage>%s</foaf:homepage>
    <foaf:name>%s</foaf:name>
  </foaf:Organization>
</dc:creator>
<dct:description xml:lang=\"%s\">%s</dct:description>
<dct:title xml:lang=\"%s\">%s</dct:title>\n"
	     conceptschemeuri
	     rights
	     timestr
	     timestr
	     type
	     source
	     subject
	     coverage
	     lang (upcase lang)
	     publisher
	     contributor
	     email homepage author
	     lang description lang title)
     (mapconcat
      (lambda (uri)
	(format "<skos:hasTopConcept rdf:resource=\"%s%s\"/>"
		conceptschemeuri
		uri))
      (org-element-map (plist-get info :parse-tree)
	  'headline (lambda (h)
		      ;; FIXME: why limiting to headlines of level 1 below?
		      (if (= (org-element-property :level h) 1)
			  (concat "#" (or (org-element-property :ID h)
					  (url-encode-url
					   (org-element-property :URI h)))))))
      "\n")
     "\n</skos:ConceptScheme>")))

(defun org-skos-template (contents info)
  "Return complete document string after SKOS conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  (concat
   (format "<?xml version=\"1.0\" encoding=\"%s\"?>\n"
	   (symbol-name org-html-coding-system))
   "<rdf:RDF
    xmlns:schema=\"http://schema.org/\"
    xmlns:owl=\"http://www.w3.org/2002/07/owl#\"
    xmlns:org=\"http://www.w3.org/ns/org#\"
    xmlns:xsd=\"http://www.w3.org/2001/XMLSchema#\"
    xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\"
    xmlns:dct=\"http://purl.org/dc/terms/\"
    xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
    xmlns:prov=\"http://www.w3.org/ns/prov#\"
    xmlns:foaf=\"http://xmlns.com/foaf/0.1/\"
    xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
    xmlns:skos=\"http://www.w3.org/2004/02/skos/core#\"
    xmlns:skosxl=\"http://www.w3.org/2008/05/skos-xl#\"
    xmlns:euvoc=\"http://publications.europa.eu/ontology/euvoc#\">"
   "\n"
   "\n"
   ;; Add description of top-level concepts
   (org-skos-build-top-level-description contents info)
   "\n"
   contents
   "</rdf:RDF>"))

(defun org-skos-section (section contents info)
  "Transcode SECTION element into SKOS format.
CONTENTS is the section contents.  INFO is a plist used as
a communication channel."
  contents)

;;; Filters

(defun org-skos-final-function (contents backend info)
  "Prettify the SKOS output."
  (with-temp-buffer
    (xml-mode)
    (insert contents)
    (indent-region (point-min) (point-max))
    (buffer-substring-no-properties (point-min) (point-max))))

(provide 'ox-skos)

;;; ox-skos.el ends here
