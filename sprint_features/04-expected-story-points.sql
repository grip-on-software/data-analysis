-- Number of expected story points
SELECT ${f(join_cols, "initial_stories")}, initial_stories.key,
	initial_stories.story_points
FROM (
    SELECT ${f(join_cols, "issue")}, ${t("issue")}.issue_id,
		${s(issue_key)} AS key, ${s(story_points)} AS story_points,
        ROW_NUMBER() OVER (
            PARTITION BY ${f(join_cols, "issue", alias=F)}, ${t("issue")}.issue_id
            ORDER BY ${f(join_cols, "issue", alias=F)}, ${t("issue")}.issue_id, ${t("issue")}.changelog_id DESC
        ) AS rev_row
    FROM gros.${t("issue")}
    JOIN gros.${t("sprint")} ON ${j(join_cols, "issue", "sprint")}
    JOIN gros.${t("issue")} AS older_issue
	ON ${j(issue_next_changelog, "issue", "older_issue")}
    JOIN (
        SELECT issue_id, sprint_id, story_points,
            MAX(updated) AS updated,
            MAX(changelog_id) AS changelog_id
        FROM gros.${t("issue")} GROUP BY issue_id, sprint_id, story_points
    ) AS later_issue
        ON ${t("issue")}.issue_id = later_issue.issue_id
        AND ${t("issue")}.sprint_id = later_issue.sprint_id
    LEFT JOIN gros.${t("issue")} AS newer_issue
	ON ${j(issue_next_changelog, "newer_issue", "later_issue")}
    LEFT JOIN gros.subtask ON ${t("issue")}.issue_id = subtask.id_subtask
	${s(issue_join)}
    WHERE (
        (older_issue.changelog_id = 0 AND older_issue.sprint_id IS NOT NULL AND issue.sprint_id = older_issue.sprint_id)
        OR (
            older_issue.updated <= ${s(planned_end)}
            AND (
                older_issue.sprint_id IS NULL
                OR older_issue.sprint_id <> issue.sprint_id
                OR older_issue.story_points IS NULL
                OR older_issue.story_points <> issue.story_points
            )
        )
    )
    AND ${story_point_types}
    AND ${t("issue")}.story_points IS NOT NULL
    AND ${t("issue")}.sprint_id IS NOT NULL
    AND ${t("issue")}.updated <= ${s(planned_late)}
    AND ${s(issue_not_done)}
    AND (
        later_issue.updated > ${s(planned_early)}
        OR later_issue.updated - ${t("issue")}.updated < ${s(planned_late)} - ${s(sprint_open)}
        OR (later_issue.updated < ${s(sprint_open)}
            AND (newer_issue.issue_id IS NULL OR newer_issue.updated > ${s(planned_late)})
        )
    )
    AND later_issue.changelog_id >= ${t("issue")}.changelog_id
    AND (
        newer_issue.issue_id IS NULL
        OR newer_issue.updated > ${s(planned_late)}
        OR (newer_issue.sprint_id = later_issue.sprint_id
            AND NOT (${s(sprint_closed)}))
    )
    AND subtask.id_parent IS NULL
) AS initial_stories
WHERE rev_row = 1
