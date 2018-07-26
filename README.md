# website-template

Haskell website template using the following:

- Scotty
- WAI session middleware
- WAI session middlware Postgres backend
- WAI static middleware (serves static files from `static/` directory)
- PGTransaction

To run:
```bash
stack image container && docker-compose -f ./docker-compose.yml up
```