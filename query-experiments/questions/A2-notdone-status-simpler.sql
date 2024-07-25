SELECT issue.story_points, COUNT(*) FROM gros.issue,
(SELECT issue_id, MAX(changelog_id) AS max_changelog_id FROM gros.issue GROUP BY issue_id) AS maxdata,
WHERE issue.issue_id = maxdata.issue_id
AND issue.changelog_id = maxdata.max_changelog_id
AND issue.resolution <> 1 AND issue.status <> 6
AND issue.story_points <> 0
GROUP BY issue.story_points
ORDER BY issue.story_points;
