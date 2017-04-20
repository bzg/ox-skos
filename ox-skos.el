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
;; #+CONCEPTSCHEMEID: set the ID of the concept scheme 
;; #+CONCEPTSCHEMEBASEURI: set the concept scheme base URI
;; #+CONCEPTSCHEMEID: set the concept scheme ID
;;
;;; Todo:
;;
;; - use :skos:note:fr: to specify the language?
;; - use :skos:note:1 or :skos:note:note_label for multiple notes?
;; - implement related (add var with list of properties?)
;; - use SKOS_EXTENSION for ttl output?
;; - write ox-skos-html to export to html
;; - write ox-skos-latex to export to latex (and scribble?!)

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

(defcustom org-skos-uri-separator "/"
  "Separate the concept scheme URI and the concept scheme ID."
  :group 'org-export-skos
  :type 'string)

(defcustom org-skos-id-separator "-"
  "Separate the concept scheme ID and the concept ID."
  :group 'org-export-skos
  :type 'string)

;; (setq org-skos-uri-separator "/")
;; (setq org-skos-id-separator "-")

(defcustom org-skos-ISO-25964 nil
  "When non-nil, include ISO-THES data."
  :group 'org-export-skos
  :type 'boolean)

;; (setq org-skos-ISO-25964 nil)

(defvar org-skos-terms nil
  "A list of terms to generate iso-thes data.")

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
    (:conceptschemebaseuri "CONCEPTSCHEMEBASEURI" nil nil t)
    (:conceptschemeid "CONCEPTSCHEMEID" nil nil t)
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
  (setq org-skos-terms nil)
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
  (setq org-skos-terms nil)
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
  (setq org-skos-terms nil)
  (org-publish-org-to
   'skos filename (concat "." org-skos-extension) plist pub-dir))

;;; Main transcoding functions

(defun org-skos-i18n (value lang attr conceptschemeuri conceptschemebaseuri)
  "Convert VALUE with LANG into xml attribute ATTR.
VALUE can be a string or an alist."
  (if (null value) ""
    (let ((values (org-babel-parse-header-arguments value))
	  (id (org-id-new))
	  (attr0 (when (string-match "[^:]+:\\(.+\\)" attr)
			 (match-string 1 attr))))
      (concat
       (when (and org-skos-ISO-25964
		  (or (string= attr0 "prefLabel")
		      (string= attr0 "altLabel")))
	 ;; Update the global list of terms
	 (push (list id attr0 values) org-skos-terms)
	 (format (concat
		  "<xl:" attr0 ">\n<iso-thes:"
		  (cond ((string= attr0 "prefLabel") "PreferredTerm")
			((string= attr0 "altLabel") "SimpleNonPreferredTerm")
			(t ""))
		  " rdf:about=\"%s" org-skos-uri-separator
		  "%s\" />\n</xl:" attr0 ">\n")
		 conceptschemebaseuri id))
       (mapconcat
	(lambda (lv)
	  (let ((l (if (cdr lv) (substring (symbol-name (car lv)) 1) lang))
		(v (if (cdr lv) (cdr lv) (symbol-name (car lv)))))
	    (format "<%s xml:lang=\"%s\">%s</%s>" attr l v attr)))
	values
	"\n")))))

(defun org-skos-headline (headline contents info)
  "Transcode HEADLINE element into SKOS format.
CONTENTS is the headline contents.  INFO is a plist used as a
communication channel."
  (let* ((id (or (org-element-property :ID headline)
		 (url-encode-url
		  (org-element-property :URI headline))))
	 (lang (org-export-data (plist-get info :language) info))
	 (timestr (format-time-string-ISO-8601))
	 ;; FIXME: check skos:scopeNote
	 (conceptschemebaseuri
	  (url-encode-url (plist-get info :conceptschemebaseuri)))
	 (conceptschemeuri
	  (concat
	   conceptschemebaseuri
	   org-skos-uri-separator
	   (url-encode-url (plist-get info :conceptschemeid))))
	 (notation
	  (org-skos-i18n
	   (org-element-property :SKOS:NOTATION headline)
	   lang "skos:notation" conceptschemeuri conceptschemebaseuri))
	 (example
	  (org-skos-i18n
	   (org-element-property :SKOS:EXAMPLE headline)
	   lang "skos:example" conceptschemeuri conceptschemebaseuri))
	 (note
	  (org-skos-i18n
	   (org-element-property :SKOS:NOTE headline)
	   lang "skos:note" conceptschemeuri conceptschemebaseuri))
	 (altlabel
	  (org-skos-i18n
	   (org-element-property :SKOS:ALTLABEL headline)
	   lang "skos:altLabel" conceptschemeuri conceptschemebaseuri))
	 (preflabel
	  (org-skos-i18n
	   (or (org-element-property :SKOS:PREFLABEL headline)
	       (org-element-property :raw-value headline))
	   lang "skos:prefLabel" conceptschemeuri conceptschemebaseuri))
	 (broader
	  (or
	   (org-element-property :ID (org-export-get-parent-headline headline))
	   (org-element-property :URI (org-export-get-parent-headline headline))))
	 (narrower  ;; a list of narrower URIs
	  (org-element-map (plist-get info :parse-tree) 'headline
	    (lambda (h)
	      (if (equal
		   (org-element-property
		    :raw-value (org-export-get-parent-headline h))
		   (org-element-property :raw-value headline))
		  (or
		   (org-element-property :ID h)
		   (org-element-property :URI h))))))
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
		lang "skos:definition" conceptschemeuri conceptschemebaseuri)
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
      "<skos:Concept rdf:about=\"%s%s%s\">
  <rdf:type rdf:resource=\"http://www.w3.org/2004/02/skos/core#Concept\"/>
  <skos:inScheme>
    <skos:ConceptScheme rdf:about=\"%s\"/>
  </skos:inScheme>
<dct:modified>%s</dct:modified>
<dct:created>%s</dct:created>
"
      conceptschemeuri org-skos-id-separator id
      conceptschemeuri
      timestr timestr)
     definition "\n" notation "\n" preflabel "\n"
     altlabel "\n" example "\n" note "\n"
     (when org-skos-ISO-25964 "<iso-thes:status>1</iso-thes:status>\n")
     ;; Possibly add "broader"
     (when broader
       (format "<skos:broader rdf:resource=\"%s%s\"/>\n" conceptschemeuri broader))
     ;; Possibly add "narrower"
     (when narrower
       (mapconcat
	(lambda (n)
	  (format "<skos:narrower rdf:resource=\"%s%s\"/>" conceptschemeuri n))
	narrower "\n"))
     ;; Possibly add topConceptOf
     (when (= (org-element-property :level headline) 1)
       (format "<skos:topConceptOf rdf:resource=\"%s\"/>" conceptschemeuri))
     ;; Possibly add iso-thes:status
     
     "\n</skos:Concept>\n"
     contents)))

;; FIXME: id should be uuid, without the conceptscheme base URI
(defun org-skos-build-iso-thes-term (term conceptschemebaseuri)
  "Use `term' to build iso-thes bloc.
`term' is a list with an id, an iso-thes attribute and a list of
cons formed from a language specified and a litteral."
  (let ((timestr (format-time-string-ISO-8601))
	(id (car term))
	(attr (nth 1 term))
	(values (nth 2 term)))
    (format
     "<iso-thes:%s rdf:about=\"%s%s%s\">
        <iso-thes:status>1</iso-thes:status>
        %s
        <dct:modified>%s</dct:modified>
        <dct:created>%s</dct:created>
    </iso-thes:%s>"
     attr
     conceptschemebaseuri org-skos-uri-separator id
     (mapconcat
      (lambda(v)
	(format "<xl:literalForm xml:lang=\"%s\">%s</xl:literalForm>"
		;; (car v)
		(substring (symbol-name (car v)) 1)
		(cdr v)))
      values "\n")
     timestr timestr
     attr)))

(defun org-skos-build-iso-thes-terms (contents info)
  "Build the list of iso-thes terms using `org-skos-terms'."
  (let ((conceptschemebaseuri (plist-get info :conceptschemebaseuri)))
    (mapconcat
     (lambda (term)
       (org-skos-build-iso-thes-term term conceptschemebaseuri))
     org-skos-terms
     "\n")))

(defun format-time-string-ISO-8601 ()
  (concat
   (format-time-string "%Y-%m-%dT%T")
   ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
    (format-time-string "%z"))))

(defun org-skos-build-top-level-description (contents info)
  (let ((conceptschemebaseuri (plist-get info :conceptschemebaseuri))
	(conceptschemeid (plist-get info :conceptschemeid))
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
     (format "<skos:ConceptScheme rdf:about=\"%s%s%s\">
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
	     conceptschemebaseuri org-skos-uri-separator conceptschemeid
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
	(format "<skos:hasTopConcept rdf:resource=\"%s%s%s%s%s\"/>"
		conceptschemebaseuri org-skos-uri-separator
		conceptschemeid org-skos-id-separator
		uri))
      (org-element-map (plist-get info :parse-tree)
	  'headline (lambda (h)
		      ;; Only consider top-level concepts
		      (if (= (org-element-property :level h) 1)
			  (or (org-element-property :ID h)
			      (url-encode-url
			       (org-element-property :URI h))))))
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
    xmlns:org=\"http://www.w3.org/ns/org#\"
    xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
    xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\"
    xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
    xmlns:dct=\"http://purl.org/dc/terms/\"
    xmlns:foaf=\"http://xmlns.com/foaf/0.1/\"
    xmlns:mcc=\"http://www.culture.fr/thesaurus/elements/1.0/\"
    xmlns:skos=\"http://www.w3.org/2004/02/skos/core#\""
   (if org-skos-ISO-25964
     "
    xmlns:xsd=\"http://www.w3.org/2001/XMLSchema#\"
    xmlns:euvoc=\"http://publications.europa.eu/ontology/euvoc#\"
    xmlns:prov=\"http://www.w3.org/ns/prov#\"
    xmlns:skosxl=\"http://www.w3.org/2008/05/skos-xl#\">"
     ">")
   "\n"
   "\n"
   ;; Add description of top-level concepts
   (org-skos-build-top-level-description contents info)
   "\n"
   contents
   (when org-skos-ISO-25964
     (org-skos-build-iso-thes-terms contents info))
   "\n</rdf:RDF>"))

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
