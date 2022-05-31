;;; custom/sql.el -*- lexical-binding: t; -*-

(defvar +sql--startable-product-list nil
  "List of start-able SQL products.")

(defvar +sql--highlightable-product-list nil
  "List of highlight-able SQL products.")

;;;###autoload
(defun +sql--populate-product-list ()
  "Update list of SQL products."
  (setq +sql--highlightable-product-list sql-product-alist
        +sql--startable-product-list
          (cl-remove-if-not (lambda (product) (sql-get-product-feature (car product) :sqli-program)) sql-product-alist)))

;;;###autoload
(defun +sql--get-product-names (products)
  "Get alist of SQL product names and symbols."
  (mapcar
   (lambda (product)
     (cons (sql-get-product-feature (car product) :name) (car product)))
   products))

;;;###autoload
(defun +sql/set-product ()
  "Set dialect-specific highlighting for buffer"
  (interactive)
  (cond ((featurep! :completion ivy)
         (ivy-read "SQL products: "
                   (+sql--get-product-names +sql--startable-product-list)
                   :require-match t
                   :action #'(lambda (product) (sql-set-product (cdr product)))
                   :caller '+sql/open-repl))))

;;;###autoload
(defun +sql/start ()
  "Set SQL dialect-specific highlighting and start inferior SQLi process."
  (interactive)
  (+sql/set-product)
  (sql-product-interactive))
