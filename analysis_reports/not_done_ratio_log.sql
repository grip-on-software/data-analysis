SELECT COALESCE(COALESCE(not_done.num_points, done.num_points), 0) AS story_points, not_done.num_not_done, done.num_done, COALESCE(not_done.num_not_done,0)/CAST(COALESCE(not_done.num_not_done,0)+COALESCE(done.num_done,0) AS double)*100 AS ratio
FROM
(
    SELECT CAST(${sprint_points_normalization}*log(CAST(issue.story_points AS double)/CAST(CASE WHEN sprint_points.max_points IS NOT NULL AND sprint_points.max_points <> 0 THEN sprint_points.max_points ELSE ${sprint_points_normalization} END AS double)) AS DECIMAL(5,2)) AS num_points, COUNT(*) AS num_not_done FROM gros.issue,
    (SELECT issue_id, MIN(changelog_id) AS first_changelog_id FROM gros.issue
        WHERE issue.type = 7
		AND issue.story_points <> 0
		AND issue.sprint_id <> 0
		AND issue.status = 3
        GROUP BY issue_id) AS points_data,
    (SELECT issue.issue_id, max_changelog_id, MIN(sprint.sprint_id) AS late_sprint FROM gros.issue, gros.sprint,
            (SELECT issue_id, sprint_id, MAX(changelog_id) AS max_changelog_id FROM gros.issue
                GROUP BY issue_id, sprint_id) AS max_data
            WHERE issue.sprint_id = sprint.sprint_id
            AND issue.issue_id = max_data.issue_id
            AND ${issue_not_done}
            AND issue.changelog_id = max_changelog_id AND sprint.end_date < CURRENT_TIMESTAMP()
            GROUP BY issue.issue_id, max_changelog_id) AS not_done_data,
    (SELECT issue.sprint_id, MAX(issue.story_points) AS max_points FROM gros.issue
        WHERE issue.story_points <= ${sprint_points_normalization}
        GROUP BY issue.sprint_id) AS sprint_points
    WHERE issue.issue_id = points_data.issue_id
    AND issue.changelog_id = points_data.first_changelog_id
    AND issue.issue_id = not_done_data.issue_id
    AND issue.sprint_id = not_done_data.late_sprint
    AND issue.sprint_id = sprint_points.sprint_id
    ${category_conditions}
    GROUP BY num_points
) AS not_done
FULL OUTER JOIN
(
    SELECT CAST(${sprint_points_normalization}*log(CAST(issue.story_points AS double)/CAST(CASE WHEN sprint_points.max_points IS NOT NULL AND sprint_points.max_points <> 0 THEN sprint_points.max_points ELSE ${sprint_points_normalization} END AS double)) AS DECIMAL(5,2)) AS num_points, COUNT(*) AS num_done FROM gros.issue,
    (SELECT issue_id, MIN(changelog_id) AS first_changelog_id FROM gros.issue
        WHERE type = 7
		AND issue.story_points <> 0
		AND issue.sprint_id <> 0
		AND issue.status = 3
        GROUP BY issue_id) AS points_data,
    (SELECT issue.issue_id, MIN(sprint.sprint_id) AS done_sprint FROM gros.issue, gros.sprint
		WHERE issue.sprint_id = sprint.sprint_id
		AND ${issue_done}
        GROUP BY issue.issue_id) AS done_data,
    (SELECT issue.sprint_id, MAX(issue.story_points) AS max_points FROM gros.issue
        WHERE issue.story_points <= ${sprint_points_normalization}
        GROUP BY issue.sprint_id) AS sprint_points
    WHERE issue.issue_id = points_data.issue_id
    AND issue.changelog_id = points_data.first_changelog_id
    AND issue.issue_id = done_data.issue_id
    AND issue.sprint_id = done_data.done_sprint
    AND issue.sprint_id = sprint_points.sprint_id
    ${category_conditions}
    GROUP BY num_points
) AS done
ON not_done.num_points = done.num_points
WHERE COALESCE(not_done.num_not_done,0)+COALESCE(done.num_done,0) <> 0
ORDER BY story_points
