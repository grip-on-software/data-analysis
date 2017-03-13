SELECT issue.project_id, issue.sprint_id, COUNT(*) AS num_links FROM gros.issue, gros.issuelink,
(SELECT issue.issue_id, MAX(issue.changelog_id) AS changelog_id FROM gros.issue GROUP BY issue.issue_id) AS maxdata
WHERE issue.issue_id = maxdata.issue_id AND issue.changelog_id = maxdata.changelog_id
AND issue.key = issuelink.from_key --OR issue.key = issuelink.to_key)
AND issue.sprint_id <> 0
GROUP BY issue.project_id, issue.sprint_id
