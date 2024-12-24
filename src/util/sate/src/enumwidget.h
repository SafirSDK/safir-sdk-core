#ifndef ENUMWIDGET_H
#define ENUMWIDGET_H

#include <QWidget>

namespace Ui {
class EnumWidget;
}

class EnumWidget : public QWidget
{
    Q_OBJECT

public:
    explicit EnumWidget(int64_t typeId, QWidget *parent);
    ~EnumWidget();

private:
    Ui::EnumWidget *ui;

    void OnSectionResized(int index, int /*oldSize*/, int newSize);
    void ApplyFilter(const QString& filterText, int column, QWidget* filterWidget);
};

#endif // ENUMWIDGET_H
