SELECT * FROM gros.issue
JOIN (SELECT issue_id, MIN(updated) AS min_updated FROM gros.issue GROUP BY issue_id) AS issue2 ON issue.issue_id = issue2.issue_id
WHERE updated_by = '' AND updated >= min_updated;
