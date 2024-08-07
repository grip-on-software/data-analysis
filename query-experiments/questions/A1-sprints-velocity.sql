SELECT project."name", cast(weekdata.start_date as date) AS sprint_start, issue.sprint_id, CAST(SUM(story_points) AS float)/(days/7*5) AS velocity
FROM gros.project,
gros.issue LEFT OUTER JOIN sprint ON issue.project_id = sprint.project_id AND issue.sprint_id = sprint.sprint_id,
(SELECT issue_id, MAX(changelog_id) AS max_changelog_id FROM gros.issue GROUP BY issue_id) AS maxdata,
(SELECT sprint_id, start_date, EXTRACT(day FROM (end_date - start_date)) AS days FROM gros.sprint) AS weekdata
-- Additional joins
WHERE issue.project_id = project.project_id
AND issue.issue_id = maxdata.issue_id AND issue.changelog_id = max_changelog_id
AND issue.sprint_id = weekdata.sprint_id
-- Resolved at the end of the sprint
AND issue.resolution = 1
GROUP BY project."name", sprint_start, issue.project_id, issue.sprint_id, days
ORDER BY issue.project_id, issue.sprint_id;
