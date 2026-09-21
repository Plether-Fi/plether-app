-- Structured Haskell records already have an integer SeverityNumber. The
-- modify filters also supply string severities for plaintext/third-party logs.
-- Do not run str_key conversion on numeric values (it logs two errors/record).
function normalize_severity(tag, timestamp, record)
  local raw = record.SeverityNumber
  local value = (type(raw) == 'string' or type(raw) == 'number') and tonumber(raw) or nil
  if value and value >= 1 and value <= 24 and value == math.floor(value) then
    -- No round-trip of already typed records: retain other integers exactly.
    if type(raw) == 'number' then return 0, timestamp, record end
    record.SeverityNumber = value
  else
    record.SeverityNumber = 9
    record.SeverityText = 'INFO'
  end
  return 2, timestamp, record
end
