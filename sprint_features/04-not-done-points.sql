SELECT DISTINCT ${f(join_cols, "issue")}, ${s(issue_key)} AS key,
    ${s(story_points)} AS story_points
FROM gros.${t("issue")}
JOIN gros.${t("sprint")} ON ${j(join_cols, "issue", "sprint")}
LEFT JOIN gros.${t("issue")} AS new_issue
ON ${j(issue_next_changelog, "new_issue", "issue")}
LEFT JOIN gros.subtask ON ${t("issue")}.issue_id = subtask.id_subtask
WHERE ${t("issue")}.story_points IS NOT NULL
AND ${t("issue")}.sprint_id IS NOT NULL
AND (
  (new_issue.issue_id IS NULL AND ${s(sprint_close)} + interval '1' day > NOW())
  OR (
    new_issue.issue_id IS NOT NULL AND new_issue.sprint_id IS NULL
    AND new_issue.updated > ${s(planned_end)}
  )
)
AND ${s(issue_not_done)}
AND subtask.id_parent IS NULL
