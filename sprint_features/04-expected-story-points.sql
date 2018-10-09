-- Number of expected story points
SELECT project_id, sprint_id, key, story_points FROM (
    SELECT issue.project_id, issue.sprint_id, issue.issue_id, issue.key, issue.story_points,
        ROW_NUMBER() OVER (
            PARTITION BY issue.project_id, issue.sprint_id, issue.issue_id
            ORDER BY issue.project_id, issue.sprint_id, issue.issue_id, issue.changelog_id DESC
        ) AS rev_row
    FROM gros.issue
    JOIN gros.sprint
        ON issue.project_id = sprint.project_id
        AND issue.sprint_id = sprint.sprint_id
    JOIN gros.issue AS older_issue
        ON issue.issue_id = older_issue.issue_id
        AND issue.changelog_id = older_issue.changelog_id + 1
    JOIN (
        SELECT issue_id, sprint_id, story_points,
            MAX(updated) AS updated,
            MAX(changelog_id) AS changelog_id
        FROM gros.issue GROUP BY issue_id, sprint_id, story_points
    ) AS later_issue
        ON issue.issue_id = later_issue.issue_id
        AND later_issue.sprint_id = issue.sprint_id
    LEFT JOIN gros.issue AS newer_issue
        ON newer_issue.issue_id = issue.issue_id
        AND newer_issue.changelog_id = later_issue.changelog_id + 1
    LEFT JOIN gros.subtask ON issue.issue_id = subtask.id_subtask
    WHERE (
        (older_issue.changelog_id = 0 AND older_issue.sprint_id IS NOT NULL AND issue.sprint_id = older_issue.sprint_id)
        OR (
            older_issue.updated <= ${planned_end}
            AND (
                older_issue.sprint_id IS NULL
                OR older_issue.sprint_id <> issue.sprint_id
				OR older_issue.story_points IS NULL
				OR older_issue.story_points <> issue.story_points
            )
        )
    )
    AND issue.story_points IS NOT NULL
    AND issue.sprint_id IS NOT NULL
    AND issue.updated <= ${planned_late}
    AND (
		later_issue.updated > ${planned_early}
		OR later_issue.updated - issue.updated < ${planned_late} - ${sprint_open}
		OR (later_issue.updated < ${sprint_open}
			AND (newer_issue.issue_id IS NULL OR newer_issue.updated > ${planned_late})
		)
	)
    AND later_issue.changelog_id >= issue.changelog_id
    AND (
        newer_issue.issue_id IS NULL
        OR newer_issue.updated > ${planned_late}
        OR (newer_issue.sprint_id = later_issue.sprint_id
			AND NOT (${sprint_closed}))
    )
    AND subtask.id_parent IS NULL
) AS initial_stories
WHERE rev_row = 1
