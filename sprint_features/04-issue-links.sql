SELECT issue.project_id, issue.sprint_id, COUNT(*) AS num_links FROM gros.issue, gros.issuelink,
(SELECT issue.issue_id, MAX(issue.changelog_id) AS changelog_id FROM gros.issue GROUP BY issue.issue_id) AS maxdata
WHERE issue.issue_id = maxdata.issue_id AND issue.changelog_id = maxdata.changelog_id
AND issue.issue_id = issuelink.id_from --OR issue.issue_id = issuelink.id_to)
AND issue.sprint_id <> 0
GROUP BY issue.project_id, issue.sprint_id
