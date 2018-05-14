-- Number of expected story points
SELECT project_id, sprint_id, SUM(story_points) AS initial_story_points FROM (
    SELECT issue.project_id, issue.sprint_id, issue.issue_id,
        MIN(issue.story_points) AS story_points
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
            older_issue.updated <= ${planned_early}
            AND (
                older_issue.sprint_id IS NULL
                OR older_issue.sprint_id <> issue.sprint_id
            )
        )
    )
    AND issue.story_points IS NOT NULL
    AND issue.sprint_id IS NOT NULL
    AND issue.updated <= ${planned_late}
    AND later_issue.updated > ${planned_early}
    AND later_issue.changelog_id >= issue.changelog_id
    AND (
        newer_issue.issue_id IS NULL
        OR newer_issue.updated > ${planned_late}
        OR newer_issue.sprint_id = later_issue.sprint_id
    )
    AND subtask.id_parent IS NULL
    GROUP BY issue.project_id, issue.sprint_id, issue.issue_id
) AS initial_stories
GROUP BY project_id, sprint_id
