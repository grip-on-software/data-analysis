SELECT sprint_devs.project_id, sprint_devs.sprint_id, COUNT(*) AS number_of_devs FROM
(SELECT commits.project_id, commits.sprint_id, commits.developer_id FROM gros.commits
	GROUP BY commits.project_id, commits.sprint_id, commits.developer_id HAVING commits.sprint_id <> 0) AS sprint_devs
GROUP BY sprint_devs.project_id, sprint_devs.sprint_id;
