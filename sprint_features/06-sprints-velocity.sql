SELECT issue.project_id, issue.sprint_id, ${velocity} AS velocity
FROM gros.issue LEFT OUTER JOIN gros.sprint ON issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id,
(SELECT issue_id, MAX(changelog_id) AS max_changelog_id FROM gros.issue GROUP BY issue_id) AS maxdata,
(SELECT sprint_id, start_date, ${sprint_days} AS sprint_days FROM gros.sprint) AS weekdata
-- Additional joins
WHERE issue.issue_id = maxdata.issue_id AND issue.changelog_id = max_changelog_id
AND issue.sprint_id = weekdata.sprint_id
-- Resolved at the end of the sprint
AND ${s(issue_done)}
GROUP BY issue.project_id, issue.sprint_id, sprint_days
ORDER BY issue.project_id, issue.sprint_id;
