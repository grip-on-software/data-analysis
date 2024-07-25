SELECT startdata.project_id, AVG(EXTRACT(day FROM end_date - start_date)) FROM
(SELECT project_id, issue_id, MIN(updated) AS start_date FROM gros.issue WHERE status = 3 GROUP BY project_id, issue_id) AS startdata,
(SELECT project_id, issue_id, MIN(updated) AS end_date FROM gros.issue WHERE resolution <> 0 GROUP BY project_id, issue_id) AS enddata
WHERE startdata.issue_id = enddata.issue_id
GROUP BY startdata.project_id ORDER BY startdata.project_id;
