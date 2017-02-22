-- Number of story watchers
SELECT issue.project_id, issue.sprint_id, AVG(issue.watchers) AS num_watchers FROM gros.issue,
(SELECT issue_id, MAX(changelog_id) AS max_changelog_id FROM gros.issue GROUP BY issue_id) AS maxdata
WHERE issue.issue_id = maxdata.issue_id
AND issue.changelog_id = maxdata.max_changelog_id
AND issue."type" = 7
AND issue.sprint_id <> 0
GROUP BY issue.project_id, issue.sprint_id
