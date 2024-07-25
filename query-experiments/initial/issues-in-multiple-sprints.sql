SELECT num_sprints, COUNT(*) FROM
(SELECT issue_id, COUNT(*) AS num_sprints FROM
	(SELECT DISTINCT issue_id, issue.sprint_id FROM gros.issue, gros.sprint
		WHERE issue.sprint_id = sprint.sprint_id
	) AS isd
	GROUP BY issue_id
) AS nsd GROUP BY num_sprints ORDER BY num_sprints;
