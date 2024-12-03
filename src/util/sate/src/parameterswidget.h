#ifndef PARAMETERSWIDGET_H
#define PARAMETERSWIDGET_H

#include <QWidget>

namespace Ui {
class ParametersWidget;
}

class ParametersWidget : public QWidget
{
    Q_OBJECT

public:
    explicit ParametersWidget(int64_t typeId, QWidget *parent);
    ~ParametersWidget();

private:
    Ui::ParametersWidget *ui;

    void OnSectionResized(int index, int /*oldSize*/, int newSize);
    void ApplyFilter(const QString& filterText, int column, QWidget* filterWidget);
};

#endif // PARAMETERSWIDGET_H
