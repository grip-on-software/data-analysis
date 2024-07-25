SELECT issue.project_id, issue.sprint_id, SUM(issue.story_points) AS num_story_points FROM gros.issue,
(SELECT issue_id, MAX(changelog_id) AS max_changelog_id FROM gros.issue GROUP BY issue_id) AS maxdata
WHERE issue.issue_id = maxdata.issue_id
AND issue.changelog_id = maxdata.max_changelog_id
AND (issue.resolution = 1 OR issue.status = 6)
AND issue.sprint_id <> 0
GROUP BY issue.project_id, issue.sprint_id;
