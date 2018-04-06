SELECT startdata.project_id, startdata.sprint_id, AVG(EXTRACT(day FROM end_date - start_date)) AS avg_duration_progress FROM
(SELECT project_id, issue_id, sprint_id, MIN(updated) AS start_date FROM gros.issue WHERE type = 7 AND status = 3 GROUP BY project_id, issue_id, sprint_id) AS startdata,
(SELECT project_id, issue_id, sprint_id, MIN(updated) AS end_date FROM gros.issue WHERE COALESCE(resolution, 0) <> 0 GROUP BY project_id, issue_id, sprint_id) AS enddata
WHERE startdata.issue_id = enddata.issue_id
GROUP BY startdata.project_id, startdata.sprint_id
