SELECT issue.issue_id, issue_changes.earliest_date,
issue_changes.status AS old_status, COALESCE(issue_changes.resolution, 0) AS old_resolution,
issue.status AS new_status, COALESCE(issue.resolution, 0) AS new_resolution,
issue.updated AS new_date
FROM gros.issue, (
	SELECT issue_id, status, resolution,
		MIN(updated) AS earliest_date, MAX(changelog_id) AS latest_changelog_id
	FROM gros.issue
	GROUP BY issue_id, status, resolution
) AS issue_changes
WHERE issue.issue_id = issue_changes.issue_id
AND issue.changelog_id = issue_changes.latest_changelog_id+1
AND ${s(issue_story_subtask)}
${category_conditions}
