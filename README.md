# Ontology Browser

## OWL API 5 migration

Version 5 targets OWL API 5.5.1 and Java 11. OWLDoc 4 embeds the owlmanager and owlhtml modules; the servlet-based owlbrowser module is also built for compatibility but is not bundled with OWLDoc.

Run `mvn --batch-mode clean verify` to build all modules and execute the unit and static-export regression tests. The legacy web module's OfflineTestCase remains skipped; the old export test tied to a developer's home directory has been replaced by deterministic temporary-directory exports.

The export tests check local links, imports, classes/properties/individuals, restrictions, Unicode and reserved characters, language-tagged annotations, anonymous ontologies, and property relationships. Static exports omit server-only Clouds and DL Query navigation. Generated page filenames use decoded UTF-8 paths with Windows-reserved characters sanitized, while HTML links remain URL-encoded.

Legacy Javadoc comments use HTML that modern doclint rejects; Javadoc generation retains doclint=none. Tracking: https://github.com/protegeproject/protege/issues/1363.

Framed exports route category links to the index pane and entity/ontology links to the main content pane. Export-only frame targeting is restored afterward so it does not affect the in-app viewer. A regression test checks frame names and link targets.
