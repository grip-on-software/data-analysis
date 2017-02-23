SELECT issue_impediment.project_id, issue_impediment.sprint_id, SUM(issue_impediment.impediment) AS num_impediments
FROM (SELECT issue.project_id, issue.sprint_id, issue.issue_id, MAX(issue.impediment) AS impediment FROM gros.issue
    WHERE issue.sprint_id <> 0
    GROUP BY issue.project_id, issue.sprint_id, issue.issue_id
) AS issue_impediment
GROUP BY issue_impediment.project_id, issue_impediment.sprint_id
