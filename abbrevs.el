;;===============;;
;; Abbreviations ;;
;;===============;;

;; Org mode
(progn
  (when (boundp 'org-mode-abbrev-table)
    (clear-abbrev-table org-mode-abbrev-table))
  (define-abbrev-table 'org-mode-abbrev-table
    '(
      ("dit" "--//--" nil 0)
      ("bahai" "bahá’í" nil 0)
      ("t1" "$T_1$" nil 0)
      ("t2" "$T_2$" nil 0)
      ("tr" "$T_R$" nil 0)
      ("te" "$T_E$" nil 0)
      ("iff" "↔" nil 0)
      ("phapy" "#+PROPERTY: header-args:Python :session *Python*" nil 0)
      )))

;; EShell
(progn
  (when (boundp 'eshell-mode-abbrev-table)
    (clear-abbrev-table eshell-mode-abbrev-table))
  (define-abbrev-table 'eshell-mode-abbrev-table
    '(
      ("ct" "ctags -e -R -f TAGS ." nil 0)
      ("ctl" "ctags -e -R -f TAGS --languages=" nil 0)
      )))

;; Elisp
(progn
  (when (boundp 'elisp-mode-abbrev-table)
    (clear-abbrev-table elisp-mode-abbrev-table))
  (define-abbrev-table 'elisp-mode-abbrev-table
    '(
      ("gsk" "(global-set-key (kbd \"" nil 0)
      ("usp" "(use-package " nil 0)
      )))

;; Python
(progn
    (when (boundp 'python-mode-abbrev-table)
      (clear-abbrev-table python-mode-abbrev-table))
    (define-abbrev-table 'python-mode-abbrev-table
      '(
        ("zia" "from __future__ import annotations")
        ("zis" "import sys" nil 0)
        ("zio" "import os" nil 0)
        ("zit" "from typing import Optional" nil 0)
        ("zim" "import multiprocessing as mp" nil 0)
        ("zil" "import logging\n\nlogger = logging.getLogger(__name__)" nil 0)
        ("zmain" "if __name__ == \"__main__\":\n")
        ("zs" "(self, " nil 0)
        ("spn" "Path | str | None = None" nil 0)
        )))

;; R
(progn
    (when (boundp 'ess-r-mode-abbrev-table)
      (clear-abbrev-table ess-r-mode-abbrev-table))
    (define-abbrev-table 'ess-r-mode-abbrev-table
      '(
        ("iis" "<-" nil 0)
        ("ggp" "plot <- ggplot" nil 0)
        ("ggs" "ggsave" nil 0)
        ("fun" "function" nil 0)
        ("dafa" "data.frame" nil 0)
        )))

;; Fortran
(progn
    (when (boundp 'fortran-mode-abbrev-table)
      (clear-abbrev-table fortran-mode-abbrev-table))
    (define-abbrev-table 'fortran-mode-abbrev-table
      '(
        ("iii" "integer, intent(in) :: " nil 0)
        ("rii" "real(dp), intent(in) :: " nil 0)
        ("iio" "integer, intent(out) :: " nil 0)
        ("rio" "real(dp), intent(out) :: " nil 0)
        ("iib" "integer, intent(inout) :: " nil 0)
        ("rib" "real(dp), intent(inout) :: " nil 0)
        ("int" "integer" nil 0)
        ("rdp" "real(dp)" nil 0)
        ("alc" "allocatable" nil 0)
        ("fun" "function" nil 0)
        )))

;; LaTeX
(progn
    (when (boundp 'latex-mode-abbrev-table)
      (clear-abbrev-table latex-mode-abbrev-table))
    (define-abbrev-table 'latex-mode-abbrev-table
      '(
        ("wrt" "with respect to" nil 0)
        ("zla" "\\label" nil 0)
        ("zca" "\\caption" nil 0)
        ("zr" "\\ref{}" nil 0)
        ("ea" "\\textit{et al.}" nil 0)
        ("td" "" (lambda () (insert
                        (format "\\todo{%s}" (read-string "Todo: ")))))
        ("tdi" "" (lambda () (insert
                         (format "\\todo[inline]{%s}" (read-string "Todo: ")))))
        ("zi" "^{(i)}" nil 0)
        ("zj" "^{(j)}" nil 0)
        ("zk" "^{(k)}" nil 0)
        ("zn" "^{(n)}" nil 0)
        ("zm" "^{(m)}" nil 0)
        ("zr" "\\mathbb{R}" nil 0)
        ("zrn" "\\mathbb{R}^n" nil 0)
        ("zc" "\\mathbb{C}" nil 0)
        ("zcn" "\\mathbb{C}^n" nil 0)
        ("iff" "↔" nil 0)
        ("txt" "" (lambda () (insert (format
                                 "\\text{%s}" (read-string "Text: ")))))
        ("partl" "" (lambda () (insert (format
                                   "\\frac{\\partial %s}{\\partial %s}"
                                   (read-string "Numerator: ")
                                   (read-string "Denominator: ")))))
        ("spartl" "" (lambda () (insert (format
                                    "\\frac{\\partial^2 %s}{\\partial %s^2}"
                                    (read-string "Numerator: ")
                                    (read-string "Denominator: ")))))
        ("inv" "" (lambda () (insert (format "\\frac{1}{%s}"
                                        (read-string "Denominator: ")))))
        )))


;;=============;;
;; Boilerplate ;;
;;=============;;

(defun boilerplate/py-sql-connecurrent ()
    (interactive)
    (insert "
def execute_sql_concurrently(
        db_path: str,
        query: Optional[str] = None,
        commit: Optional[bool] = False,
        fetchone: Optional[bool] = False,
        max_tries: Optional[int] = 10,
        timeout: Optional[int] = 10,
) -> tuple:
    \"\"\"Executes an SQL command concurrently

    Args:
        db_path (str) : Path to SQLite3 database.
        query (str, optional) : Query to execute.
        df_table (str, optional) : Table to write to if dataframe is present.
        commit (bool, optional) : If True, will perform a commit after the query.
        fetchone (bool, optional) : If True, will return only the first result.
                Defaults to False.
        max_tries (int, optional) : Maximum number of retries for SQL connection
                If <0, will perpetually retry. Defaults to -1.
        timeout (int, optional) : Timeout for SQLite connection in seconds.
                Defaults to 10

    Returns:
        result (list) : Result from the SQL query.
    \"\"\"

    db_opt_sucessful = False
    tries = -1

    while not db_opt_sucessful:
        tries += 1
        try:
            con = sqlite3.connect(db_path, timeout=timeout)
            if query is not None:
                cursor = con.cursor()
                cursor.execute(query)

                if fetchone:
                    result = cursor.fetchone()[0]
                else:
                    result = cursor.fetchall()

                if commit:
                    con.commit()

            con.close()
            db_opt_sucessful = True

        except sqlite3.OperationalError:
            if tries >= max_tries and max_tries >= 0:
                logger.critical(
                    \"Maximum SQLite3 tries exceed \"
                    f\"({tries}/{max_tries})\"
                )
                raise

    return result"))

(defun boilerplate/py-argparser ()
  (interactive)
  (insert "
parser = argparse.ArgumentParser(
        description=\"\"
     )

args = parser.parse_args()"))

(defun boilerplate/py-parser-add-arg ()
  (interactive)
  (insert "
parser.add_argument(
        \"--arg\",
        default=None,
        type=int,
        help=\"\",
    )"))

(defun boilerplate/py-parser-add-log ()
  (interactive)
  (insert "
parser.add_argument(
        \"--log\",
        default=\"warning\",
        type=str,
        help=\"Log level, can be debug, info, warning, error or critical.\",
    )

args = parser.parse_args()
log_level = getattr(logging, args.log.upper())
logging.basicConfig(level=log_level)"))
