SELECT issue.project_id, issue.sprint_id, SUM(issue.attachments) AS num_attachments FROM gros.issue,
    (SELECT issue.issue_id, MAX(issue.changelog_id) AS changelog_id
    FROM gros.issue GROUP BY issue.issue_id) AS maxdata
WHERE issue.issue_id = maxdata.issue_id AND issue.changelog_id = maxdata.changelog_id
AND issue.sprint_id <> 0
GROUP BY issue.project_id, issue.sprint_id
