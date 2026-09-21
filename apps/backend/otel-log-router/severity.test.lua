dofile('severity.lua')
for n = 1, 24 do
  local record = {SeverityNumber=n, SeverityText='ERROR', sentinel={value='unchanged'}}
  local code, ts, result = normalize_severity('fixture', 123, record)
  assert(code == 0 and ts == 123 and result == record and result.SeverityNumber == n)
  record.SeverityNumber = tostring(n)
  code, ts, result = normalize_severity('fixture', 123, record)
  assert(code == 2 and ts == 123 and result.SeverityNumber == n)
  assert(result.sentinel.value == 'unchanged' and result.SeverityText == 'ERROR')
end
for _, value in ipairs({false, {}, '', 'secret', 0, 25, 1.5, math.huge, 0/0}) do
  local code, _, result = normalize_severity('fixture', 123, {SeverityNumber=value})
  assert(code == 2 and result.SeverityNumber == 9 and result.SeverityText == 'INFO')
end
local _, _, missing = normalize_severity('fixture', 123, {})
assert(missing.SeverityNumber == 9)
print('severity normalization tests passed')
