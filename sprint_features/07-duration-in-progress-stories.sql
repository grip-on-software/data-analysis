SELECT startdata.project_id, startdata.sprint_id, startdata.key, enddata.story_points, EXTRACT(day FROM enddata.end_date - startdata.start_date) AS num FROM
(SELECT project_id, issue_id, key, sprint_id, MIN(updated) AS start_date FROM gros.issue WHERE ${issue_story} AND status = 3 GROUP BY project_id, issue_id, key, sprint_id) AS startdata,
(SELECT project_id, issue_id, key, sprint_id, MAX(story_points) AS story_points, MIN(updated) AS end_date FROM gros.issue WHERE ${s(issue_done)} GROUP BY project_id, issue_id, key, sprint_id) AS enddata, gros.sprint
WHERE startdata.issue_id = enddata.issue_id
AND startdata.sprint_id = enddata.sprint_id
AND startdata.project_id = sprint.project_id
AND startdata.sprint_id = sprint.sprint_id
AND enddata.end_date > startdata.start_date
AND enddata.end_date <= ${sprint_close} + (sprint.end_date - sprint.start_date)/7
