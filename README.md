# cl-stack-tzdata

IANA [tzdb](https://www.iana.org/time-zones) as a Lisp-loadable package for [cl-stack](https://github.com/egao1980/cl-stack).

| System | Nick | Role |
|--------|------|------|
| `cl-stack-tzdata` | `stack-tzdata` / `tzdata` | TZif v2/v3 parser, zone index, aliases, abbreviation index |

## Data

- `data/zoneinfo/**` — fat TZif (full history + abbreviations) from `zic -b fat`
- `data/links.sexp` — alias → canonical map (`Europe/Kiev` → `Europe/Kyiv`, …)
- `data/abbreviations.sexp` — historical abbreviation → validity windows
- `data/VERSION` — tzdb release id (`2026c`); ASDF/OCI version `2026.3.0` (letter → minor)

## Usage

```lisp
(asdf:load-system "cl-stack-tzdata")
(tzdata:find-zone "Europe/Kiev")           ; → canonical Europe/Kyiv
(tzdata:zone-offset-at "Europe/London" 1594818000)  ; → 3600 T "BST"
(tzdata:resolve-abbreviation "BST" 1594818000 :zone-hints '("Europe/London"))
```

## Update pipeline

```bash
# CI: .github/workflows/update-tzdata.yml
# Dogfoods cl-stack-http × http-backend-async × event-backend-libuv; zic/tar for compile.
ros -l scripts/ci-install-update.lisp -q
ros -l scripts/update-tzdata.lisp -q
ros -l scripts/gen-abbreviations.lisp -q
```

## License

MIT. Zone data © IANA (public domain / CC0-style redistribution).
