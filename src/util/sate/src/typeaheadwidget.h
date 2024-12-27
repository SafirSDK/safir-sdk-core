#ifndef TYPEAHEADWIDGET_H
#define TYPEAHEADWIDGET_H

#include <QWidget>
#include <QLineEdit>
#include <QListView>
#include <QSortFilterProxyModel>


class TypeAheadWidget : public QLineEdit
{
    Q_OBJECT

public:
    explicit TypeAheadWidget(QWidget *parent);
    ~TypeAheadWidget();

    void SetModel(QSortFilterProxyModel* model);
    QSortFilterProxyModel* Model() const;

signals:
    void ItemSelected(const QModelIndex& selectedIndex);

protected:
    bool eventFilter(QObject *obj, QEvent *event) override;

private:
    QListView* m_listView;
    QSortFilterProxyModel* m_model = nullptr;

    int m_dropdownRowWidth = 0;
    int m_dropdownRowHeight = 0;

    void OnTextEdited(const QString& text);
};

#endif // TYPEAHEADWIDGET_H
