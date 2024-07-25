SELECT project_id, key, status, resolution, expected_ltcs, story_points FROM gros.issue,
(SELECT issue_id, MAX(changelog_id) AS max_changelog_id FROM gros.issue GROUP BY issue_id) AS maxdata
WHERE issue.issue_id = maxdata.issue_id AND issue.changelog_id = maxdata.max_changelog_id
AND expected_ltcs > 0 AND story_points IS NOT NULL
