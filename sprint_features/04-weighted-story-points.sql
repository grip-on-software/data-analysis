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
        AND ${f(join_cols, "issue", mask=2)} <> 0
        AND ${s(issue_in_progress)} -- issue was in progress during this sprint
    GROUP BY issue_id
) AS points_data
ON ${j(issue_changelog, "issue", "points_data")}
JOIN (SELECT
    ${t("issue")}.issue_id, MIN(${f(join_cols, "issue", mask=2, alias=F)}) AS done_sprint
    FROM gros.${t("issue")}, gros.${t("sprint")}
    WHERE ${j(join_cols, "issue", "sprint")}
    AND ${s(issue_done)}
    GROUP BY ${t("issue")}.issue_id
) AS done_data
ON ${t("issue")}.issue_id = done_data.issue_id
AND ${f(join_cols, "issue", "done_data", mask=2)}
-- TODO: Weight the maximum points over the last three sprints
JOIN (SELECT
	${f(join_cols, "issue", mask=2)}, MAX(${s(story_points)}) AS max_points
	FROM gros.${t("issue")}
    --WHERE ${t("issue")}.story_points <= sprint_points_normalization
    ${g(join_cols, "issue", mask=2)}
) AS sprint_points
ON ${j(join_cols, "issue", "sprint_points", mask=2)}
${s(issue_join)}
${g(join_cols, "issue")}
--ORDER BY num_weighted_points
