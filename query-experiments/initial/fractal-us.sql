SELECT SUM(story_points) FROM gros.issue
JOIN (SELECT issue_id, MAX(changelog_id) AS changelog_id FROM gros.issue GROUP BY issue_id) AS latest_issue
ON issue.issue_id = latest_issue.issue_id AND issue.changelog_id = latest_issue.changelog_id
WHERE
(issue.status NOT IN (5, 6) OR issue.resolution NOT IN (2,3,4,5,7,17,18,19,20))
--AND project_id = <select project ID here>
