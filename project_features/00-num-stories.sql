-- Number of stories in total
SELECT ${f(join_cols, "issue")}, COUNT(DISTINCT issue_id) AS num_stories
FROM gros.${t("issue")}
WHERE ${s(issue_story)}
${g(join_cols, "issue")}
