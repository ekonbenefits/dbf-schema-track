# dbf-schema-track
Pre-commit program for schema tracking

This program allows you to track the schema of dbf files that are in another directory or ignored from your code versioning.

Using a pre-commit hook it writes out a text files describing your dbf schema, and checks if there are any changes,
giving you the chance to commit them in.
