SELECT COALESCE(not_done.story_points, done.story_points) AS story_points, not_done.num_not_done, done.num_done, COALESCE(not_done.num_not_done,0)/CAST(COALESCE(not_done.num_not_done,0)+COALESCE(done.num_done,0) AS double)*100 AS ratio
FROM
(
    SELECT issue.story_points, COUNT(*) AS num_not_done FROM gros.issue,
    (SELECT issue_id, MIN(changelog_id) AS first_changelog_id FROM gros.issue
        WHERE issue.type = 7 AND issue.sprint_id <> 0 AND issue.story_points <> 0 AND issue.status = 3
        GROUP BY issue_id) AS points_data,
    (SELECT issue.issue_id, max_changelog_id, MIN(sprint.sprint_id) AS late_sprint FROM gros.issue, gros.sprint,
            (SELECT issue_id, sprint_id, MAX(changelog_id) AS max_changelog_id FROM gros.issue
                GROUP BY issue_id, sprint_id) AS max_data
            WHERE issue.sprint_id = sprint.sprint_id
            AND issue.issue_id = max_data.issue_id
            AND issue.resolution <> 1 AND issue.status <> 6
            AND issue.changelog_id = max_changelog_id AND sprint.end_date < CURRENT_TIMESTAMP()
            GROUP BY issue.issue_id, max_changelog_id) AS not_done_data
    WHERE issue.issue_id = points_data.issue_id
    AND issue.changelog_id = points_data.first_changelog_id
    AND issue.issue_id = not_done_data.issue_id
    AND issue.sprint_id = not_done_data.late_sprint
    --AND issue.project_id IN (<select project IDs here>)
    GROUP BY issue.story_points
) AS not_done
FULL OUTER JOIN
(
    SELECT issue.story_points, COUNT(*) AS num_done FROM gros.issue,
    (SELECT issue_id, MIN(changelog_id) AS first_changelog_id FROM gros.issue
        WHERE type = 7 AND issue.story_points <> 0 AND issue.sprint_id <> 0 AND issue.status = 3
        GROUP BY issue_id) AS points_data,
    (SELECT issue.issue_id, MIN(sprint.sprint_id) AS done_sprint FROM gros.issue, gros.sprint
    WHERE issue.sprint_id = sprint.sprint_id
    AND (issue.resolution = 1 OR issue.status = 6)
        GROUP BY issue.issue_id) AS done_data
    WHERE issue.issue_id = points_data.issue_id
    AND issue.changelog_id = points_data.first_changelog_id
    AND issue.issue_id = done_data.issue_id
    AND issue.sprint_id = done_data.done_sprint
    --AND issue.project_id IN (<select project IDs here>)
    GROUP BY issue.story_points
) AS done
ON not_done.story_points = done.story_points
ORDER BY story_points;
