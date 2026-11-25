# Ontology Browser

## OWL API 5 migration

Version 5 targets OWL API 5.5.1 and Java 11. OWLDoc 4 embeds the owlmanager and owlhtml modules; the servlet-based owlbrowser module is also built for compatibility but is not bundled with OWLDoc.

Run `mvn --batch-mode clean verify` to build all modules and execute the unit and static-export regression tests. The legacy web module's OfflineTestCase remains skipped; the old export test tied to a developer's home directory has been replaced by deterministic temporary-directory exports.

The export tests check local links, imports, classes/properties/individuals, restrictions, Unicode and reserved characters, language-tagged annotations, anonymous ontologies, and property relationships. Static exports omit server-only Clouds and DL Query navigation. Generated page filenames use decoded UTF-8 paths with Windows-reserved characters sanitized, while HTML links remain URL-encoded.

Legacy Javadoc comments use HTML that modern doclint rejects; Javadoc generation retains doclint=none. Tracking: https://github.com/protegeproject/protege/issues/1363.

Static exports use a frameless layout with a searchable, resizable sidebar, category filters, SVG resource icons, and individual page URLs. A local JavaScript index supports name/IRI search without a server or external assets, including file:// exports. Navigation preserves the search, category, sidebar width, and list scroll position. Without JavaScript, the underlying documentation and index links remain available. Export configuration is restored afterward so the in-app viewer keeps its existing behavior.

Regression tests also verify the local search index and its page targets, frameless output, configuration restoration on success/failure, and refreshed entity pages when exporting again to the same directory.

Section rendering snapshots asserted and inferred collections once per render and reuses them for row styling and counts. The snapshots are released after each render, including failures, so subsequent ontology edits and selection changes remain visible. Regression tests cover bounded query counts, inferred/asserted overlap, ontology selection, and repeat rendering.

Annotation-assertion subjects use type-specific signature indexes rather than scanning every entity for each row. Annotation-property and datatype lookups preserve entities referenced only in ontology annotations. A regression test preserves punned subjects while rejecting the two expensive generic lookup APIs.

Static exports paginate Usage lists larger than 500 references. The entity page contains the first 500 references, with further pages linked by ordinary offline-compatible Previous/Next and numbered links. All references are retained; each extra page keeps the entity sidebar and context. Cancellation is checked between usage pages. The in-app viewer keeps its existing rendering.
