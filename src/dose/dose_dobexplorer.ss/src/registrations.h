#ifndef REGISTRATIONS_H
#define REGISTRATIONS_H

#include <Safir/Dob/Typesystem/Internal/DistributionScopeReader.h>
#include <QTimer>
#include <QWidget>
#include <vector>
#include <memory>

namespace Ui {
class registrations;
}

class Registrations : public QWidget
{
    Q_OBJECT

public:
    explicit Registrations(QWidget *parent = nullptr);
    ~Registrations();

    struct RegData
    {
        int64_t typeId;
        QString typeName;
        QString handler;
        QString connectionName;
        int context;
        bool pending;
        Safir::Dob::DistributionScope::Enumeration scope;
        QString content;
    };

public slots:
    void FilterChanged(const QString&);
    void Update();

private:
    Ui::registrations *ui;
    QTimer m_timer;

    // Read all registration data
    void UpdateRegistartionData();

    // update rows in TableWidget
    void UpdateGui();

    // Apply filter text
    void ApplyFilter();

    Safir::Dob::Typesystem::Internal::DistributionScopeReader m_distributionScopeReader;
    std::vector<std::shared_ptr<RegData>> m_regdata;
    std::vector<std::shared_ptr<RegData>> m_regdataFiltered;
};

#endif // REGISTRATIONS_H
