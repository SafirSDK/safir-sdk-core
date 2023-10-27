/******************************************************************************
*
* Copyright Saab AB, 2023 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson
*
*******************************************************************************
*
* This file is part of Safir SDK Core.
*
* Safir SDK Core is free software: you can redistribute it and/or modify
* it under the terms of version 3 of the GNU General Public License as
* published by the Free Software Foundation.
*
* Safir SDK Core is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
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
        bool detached;
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
