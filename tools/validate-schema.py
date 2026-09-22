"""Validate every emitted fixture, then verify important invalid shapes fail."""
import json
from pathlib import Path
import jsonschema
schema = json.loads(Path('inst/schema/bridge-v1.json').read_text())
validator = jsonschema.Draft202012Validator(schema, format_checker=jsonschema.FormatChecker())
for path in Path('inst/fixtures').glob('*.json'):
    validator.validate(json.loads(path.read_text()))
for mutation in ({'contract': '1'}, {'data': {'items': {}, 'truncated': False}}, {'secret': 'unexpected'}):
    value = json.loads(Path('inst/fixtures/products.json').read_text())
    value.update(mutation)
    assert not validator.is_valid(value), mutation
print('Validated all actual R fixtures and rejected malformed DTOs.')
