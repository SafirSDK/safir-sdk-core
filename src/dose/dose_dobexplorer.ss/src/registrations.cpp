#include "registrations.h"
#include "ui_registrations.h"

#include <wchar.h>
#include <sstream>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/Properties.h>
#include <Safir/Dob/Typesystem/Parameters.h>
#include <Safir/Dob/DistributionScopeProperty.h>
#include <Safir/Dob/DistributionScopeOverrideProperty.h>

namespace
{
    // Create read-only table item
    QTableWidgetItem* TableItem(QString text, bool alignCenter = false)
    {
        auto item = new QTableWidgetItem(text);
        item->setFlags(item->flags() &  ~Qt::ItemIsEditable);
        if (alignCenter)
        {
            item->setTextAlignment(Qt::AlignCenter);
        }
        return item;
    }

    QString Handler(const std::wstring& name, int64_t val)
    {
        if (name.size() > 0)
        {
            return QString::fromWCharArray(name.c_str());
        }
        return QString::number(val);
    }

    template<typename RegT>
    std::shared_ptr<Registrations::RegData> ToRegData(const Safir::Dob::Internal::ConnectionPtr& connectionPtr, const RegT& regInfo, const std::vector<int64_t> localTypes)
    {
        auto r = std::make_shared<Registrations::RegData>();
        r->connectionName = connectionPtr->NameWithCounter();
        r->typeId = regInfo.typeId;
        r->typeName = QString::fromWCharArray(Safir::Dob::Typesystem::Operations::GetName(regInfo.typeId).c_str());
        r->handler = Handler(regInfo.handlerId.GetRawString(), regInfo.handlerId.GetRawValue());
        r->context = connectionPtr->Id().m_contextId;
        r->pending = false;
        r->local = std::binary_search(begin(localTypes), end(localTypes), regInfo.typeId);

        std::ostringstream os;
        os << r->connectionName.toStdString() << " " << r->typeName.toStdString() << " " << r->typeId << " " << r->handler.toStdString();
        r->content = os.str().c_str();
        return r;
    }

    static std::vector<Safir::Dob::Typesystem::TypeId> CalculateLocalTypes()
    {
        bool isInherited, hasProperty;
        DotsC_TypeId paramTypeId;
        DotsC_ParameterIndex paramId;
        DotsC_Int32 paramIndex;

        std::vector<Safir::Dob::Typesystem::TypeId> localTypes;
        for (auto typeId : Safir::Dob::Typesystem::Operations::GetAllTypeIds())
        {

            Safir::Dob::Typesystem::Operations::HasProperty(typeId,
                                                     Safir::Dob::DistributionScopeOverrideProperty::ClassTypeId,
                                                     hasProperty,
                                                     isInherited);

            if (hasProperty && !isInherited)
            {
                Safir::Dob::Typesystem::Properties::GetParameterReference(typeId, Safir::Dob::DistributionScopeOverrideProperty::ClassTypeId, 0, 0, paramTypeId, paramId, paramIndex);
                auto distributionScope = static_cast<Safir::Dob::DistributionScope::Enumeration>(Safir::Dob::Typesystem::Parameters::GetEnumeration(paramTypeId, paramId, paramIndex));

                if (distributionScope == Safir::Dob::DistributionScope::Local)
                {
                    localTypes.push_back(typeId);
                }
            }
            else if (Safir::Dob::Typesystem::Operations::HasProperty(typeId, Safir::Dob::DistributionScopeProperty::ClassTypeId))
            {
                Safir::Dob::Typesystem::Properties::GetParameterReference(typeId, Safir::Dob::DistributionScopeProperty::ClassTypeId, 0, 0, paramTypeId, paramId, paramIndex);
                auto distributionScope = static_cast<Safir::Dob::DistributionScope::Enumeration>(Safir::Dob::Typesystem::Parameters::GetEnumeration(paramTypeId, paramId, paramIndex));

                if (distributionScope == Safir::Dob::DistributionScope::Local)
                {
                    localTypes.push_back(typeId);
                }
            }
        }

        std::sort(localTypes.begin(),localTypes.end());
        localTypes.shrink_to_fit();
        return localTypes;
    }
}


Registrations::Registrations(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::registrations),
    m_timer(this),
    m_localTypes(CalculateLocalTypes())
{
    ui->setupUi(this);

    ui->tableWidget->resizeRowsToContents();
    ui->tableWidget->resizeColumnsToContents();
    ui->tableWidget->setAlternatingRowColors(true);

    connect(ui->filterEdit,
            SIGNAL(textChanged(const QString &)),
            this,
            SLOT(FilterChanged(const QString&)));

    Update();

    ui->tableWidget->resizeColumnsToContents();

    connect(&m_timer, SIGNAL(timeout()), this, SLOT(Update()));
    m_timer.start(3000);
}

Registrations::~Registrations()
{
    delete ui;
}

void Registrations::FilterChanged(const QString&)
{
    ApplyFilter();
    UpdateGui();
}

void Registrations::Update()
{
    UpdateRegistartionData();
    ApplyFilter();
    UpdateGui();
}

void Registrations::ApplyFilter()
{
    m_regdataFiltered.clear();
    auto filterText = ui->filterEdit->text().trimmed();
    if (filterText.isEmpty())
    {
        m_regdataFiltered.insert(begin(m_regdataFiltered), begin(m_regdata), end(m_regdata));
        return;
    }

    for (const auto& ptr : m_regdata)
    {
        if (ptr->content.contains(filterText, Qt::CaseInsensitive))
        {
            m_regdataFiltered.push_back(ptr);
        }
    }
}

void Registrations::UpdateGui()
{
    ui->tableWidget->setSortingEnabled(false);
    ui->tableWidget->setRowCount(m_regdataFiltered.size());
    int row = 0;
    for (const auto& r : m_regdataFiltered)
    {
        ui->tableWidget->setItem(row, 0, TableItem(r->typeName));
        ui->tableWidget->setItem(row, 1, TableItem(r->handler));
        ui->tableWidget->setItem(row, 2, TableItem(r->connectionName));
        ui->tableWidget->setItem(row, 3, TableItem(QString::number(r->context), true));
        ui->tableWidget->setItem(row, 4, TableItem(r->pending ? "Pending" : "", true));
        ui->tableWidget->setItem(row, 5, TableItem(r->local ? "Local" : "", true));
        ++row;
    }
    ui->tableWidget->setSortingEnabled(true);
}

void Registrations::UpdateRegistartionData()
{
    m_regdata.clear();
    Safir::Dob::Internal::Connections::Instance().ForEachConnectionPtr([this](const auto& connectionPtr)
    {
        // Active registrations
        for (const auto& regInfo : connectionPtr->GetRegisteredHandlers())
        {
            auto r = ToRegData(connectionPtr, regInfo, m_localTypes);
            m_regdata.push_back(r);
        }

        // Pending registrations
        for (const auto& pendingReg : connectionPtr->GetPendingRegistrations())
        {
            if (!pendingReg.accepted && !pendingReg.remove)
            {
                auto r = ToRegData(connectionPtr, pendingReg, m_localTypes);
                r->pending = true;
                m_regdata.push_back(r);
            }
        }
    });
}
