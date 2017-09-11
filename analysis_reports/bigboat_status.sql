SELECT
  project_id,
  name,
  checked_date,
  ok,
  value,
  max
FROM gros.bigboat_status
ORDER BY project_id, checked_date, name
