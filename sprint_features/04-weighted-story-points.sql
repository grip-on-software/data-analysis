SELECT ${f(join_cols, "issue")}, SUM(${s(story_points)}) AS num_points,
    SUM(CAST(
        CAST(${s(story_points)} AS double) / 
        CAST(
            CASE WHEN sprint_points.max_points IS NOT NULL
            AND sprint_points.max_points <> 0
            THEN sprint_points.max_points
            ELSE 1
            END
        AS double)
    AS DECIMAL(5,2))) AS num_weighted_points,
    COUNT(*) AS num_done 
FROM gros.${t("issue")}
JOIN (SELECT
    issue_id, MIN(changelog_id) AS changelog_id -- first changelog ID
    FROM gros.${t("issue")}
    WHERE ${s(issue_story_subtask)} -- only count story and subtask points
        AND ${t("issue")}.story_points <> 0
        AND ${t("issue")}.sprint_id <> 0
        AND ${s(issue_in_progress)} -- issue was in progress during this sprint
    GROUP BY issue_id
) AS points_data
ON ${j(issue_changelog, "issue", "points_data")}
JOIN (SELECT
    ${t("issue")}.issue_id, MIN(${t("sprint")}.sprint_id) AS done_sprint
    FROM gros.${t("issue")}, gros.${t("sprint")}
    WHERE ${j(join_cols, "issue", "sprint")}
    AND ${s(issue_done)}
    GROUP BY ${t("issue")}.issue_id
) AS done_data
ON ${t("issue")}.issue_id = done_data.issue_id
AND ${t("issue")}.sprint_id = done_data.done_sprint
-- TODO: Weight the maximum points over the last three sprints
JOIN (SELECT
	${t("issue")}.sprint_id, MAX(${s(story_points)}) AS max_points
	FROM gros.${t("issue")}
    --WHERE ${t("issue")}.story_points <= sprint_points_normalization
    GROUP BY ${t("issue")}.sprint_id
) AS sprint_points
ON ${t("issue")}.sprint_id = sprint_points.sprint_id
${s(issue_join)}
${g(join_cols, "issue")}
--ORDER BY num_weighted_points
