"""Validate every emitted fixture, then verify important invalid shapes fail."""
import json
from pathlib import Path
import jsonschema
schema = json.loads(Path('inst/schema/bridge-metadata-v1.json').read_text())
validator = jsonschema.Draft202012Validator(schema, format_checker=jsonschema.FormatChecker())
schema_v2 = json.loads(Path('inst/schema/bridge-diagnostics-v1.json').read_text())
validator_v2 = jsonschema.Draft202012Validator(schema_v2, format_checker=jsonschema.FormatChecker())
for path in Path('inst/fixtures').glob('*.json'):
    value = json.loads(path.read_text())
    (validator_v2 if value.get('contract') == 2 else validator).validate(value)
detail = json.loads(Path('inst/fixtures/product.json').read_text())
assert 'guarantees' in detail['data'], 'Bridge fixtures must exercise port guarantees'
leaked = json.loads(json.dumps(detail))
leaked['data']['guarantees']['endpoint'] = 'private destination'
assert not validator.is_valid(leaked), 'Product guarantee must not expose target endpoints'
for mutation in ({'contract': '1'}, {'data': {'items': {}, 'truncated': False}}, {'secret': 'unexpected'}):
    value = json.loads(Path('inst/fixtures/products.json').read_text())
    value.update(mutation)
    assert not validator.is_valid(value), mutation
print('Validated all actual R fixtures and rejected malformed DTOs.')

value = json.loads(Path('inst/fixtures/diagnostics-v2.json').read_text())
assert len(value['data']['items']) == 1, 'Real sourced failure must produce a location'
assert not validator.is_valid(value), 'v2 cannot be mistaken for v1'
for mutation in ({'file_hash': 'bad'}, {'source': 'must not leak'}, {'start': {'line': -1, 'character': 0}}, {'severity': None}):
    bad = json.loads(json.dumps(value))
    bad['data']['items'][0].update(mutation)
    assert not validator_v2.is_valid(bad), mutation
error = json.loads(Path('inst/fixtures/diagnostics-error-v2.json').read_text())
assert error['kind'] == 'error' and error['error']['code'] == 'not_found'
print('Validated explicit v2 diagnostics and structured errors without changing v1.')
