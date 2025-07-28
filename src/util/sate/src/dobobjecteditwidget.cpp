/******************************************************************************
*
* Copyright Saab AB, 2024 (http://safirsdkcore.com)
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
#include "dobobjecteditwidget.h"
#include "qmessagebox.h"
#include "ui_dobobjecteditwidget.h"
#include "dobobjectmodel.h"
#include "dobobjectdelegate.h"
#include "dobobjectbuilder.h"
#include "utilities.h"
#include <QTreeView>
#include <QKeyEvent>
#include <QComboBox>
#include <QTimer>
#include <QScrollBar>
#include <QSortFilterProxyModel>
#include <QDebug>
#include <QMessageBox>
#include <QClipboard>
#include <Safir/Dob/Typesystem/Serialization.h>

struct ExpandedRow
{
    int row = -1;
    std::vector<ExpandedRow> subRows;

    void Debug(const QString& indent) const
    {
        qDebug() << indent << row;
        for (const auto& c : subRows)
        {
            c.Debug(indent + " ");
        }
    }
};

class ObjectSortFilterProxyModel : public QSortFilterProxyModel
{
public:
    ObjectSortFilterProxyModel(QWidget* parent) : QSortFilterProxyModel(parent)
    {
        setRecursiveFilteringEnabled(true); // show parent nodes when a child node is matching a search filter
    }

    void setFilterRegularExpression(const int column, QRegularExpression&& regex)
    {
        m_filters[column] = std::move(regex);
        invalidateFilter();
    }

protected:
    bool filterAcceptsRow(int sourceRow, const QModelIndex& sourceParent) const override
    {
        for (int i = 0; i < 5; i++)
        {
            if (m_filters[i].isValid())
            {
                auto ix = sourceModel()->index(sourceRow, i, sourceParent);
                auto data = sourceModel()->data(ix, filterRole()).toString();
                if (!m_filters[i].match(data).hasMatch())
                {
                    return false;
                }
            }
        }

        return true;
    }

private:
    QRegularExpression m_filters[5];
};


DobObjectEditWidget::DobObjectEditWidget(DobHandler* dob, int64_t typeId, QWidget *parent)
    : QWidget(parent)
    , ui(new Ui::DobObjectEditWidget)
    , m_dob(dob)
    , m_typeId(typeId)
{
    m_sourceModel = new DobObjectModel(m_typeId, this);

    Init();

    auto ch = GetDefaultValueForChannelOrHandler();
    if (!ch.isNull())
    {
        ui->operationsWidget->idEdit->setText(ch);
    }
}

DobObjectEditWidget::DobObjectEditWidget(DobHandler* dob, QString channelHandler, int64_t instance, const Safir::Dob::Typesystem::ObjectPtr& object,  QWidget *parent)
    : QWidget(parent)
    , ui(new Ui::DobObjectEditWidget)
    , m_dob(dob)
    , m_typeId(object->GetTypeId())
{
    m_sourceModel = new DobObjectModel(m_typeId, object, this);

    Init();

    if (!channelHandler.isNull() && !channelHandler.isEmpty())
    {
        ui->operationsWidget->idEdit->setText(channelHandler);
    }
    else
    {
        auto ch = GetDefaultValueForChannelOrHandler();
        if (!ch.isNull())
        {
            ui->operationsWidget->idEdit->setText(ch);
        }
    }

    if (instance != -1)
    {
        ui->operationsWidget->instanceEdit->setText(QString::number(instance));
    }
}

QString DobObjectEditWidget::GetDefaultValueForChannelOrHandler() const
{
    auto cls = TypesystemRepository::Instance().GetClass(m_typeId);
    if (cls != nullptr)
    {
        if (cls->dobBaseClass == TypesystemRepository::Entity || cls->dobBaseClass == TypesystemRepository::Service)
        {
            auto reg = m_dob->GetMyRegistration(m_typeId);
            if (reg != nullptr)
            {
                // If we are registered handler, set HandlerId to the one we have registered.
                return QString::fromStdWString(reg->handler.ToString());
            }
            return "DEFAULT_HANDLER";
        }
        else if (cls->dobBaseClass == TypesystemRepository::Message)
        {
            return "DEFAULT_CHANNEL";
        }
    }

    return {};
}

DobObjectEditWidget::~DobObjectEditWidget()
{
    delete ui;
}

void DobObjectEditWidget::Init()
{
    ui->setupUi(this);

    auto proxyModel = new ObjectSortFilterProxyModel(this);
    proxyModel->setSourceModel(m_sourceModel);
    proxyModel->setFilterRole(DobObjectModel::FilterRole);
    ui->objectEditTreeView->setModel(proxyModel);

    ui->objectEditTreeView->setItemDelegateForColumn(1, new DobObjectDelegate());
    ui->objectEditTreeView->setItemDelegateForColumn(3, new CheckboxDelegate());

    ui->objectEditTreeView->resizeColumnToContents(0);
    auto col0Width = ui->objectEditTreeView->columnWidth(0);
    ui->objectEditTreeView->setColumnWidth(0, col0Width > 200 ? col0Width : 200); // Min start width at least 200 to make it look better
    ui->objectEditTreeView->setColumnWidth(1, 300);
    ui->objectEditTreeView->setColumnWidth(2, 80);
    ui->objectEditTreeView->setColumnWidth(3, 80);
    ui->objectEditTreeView->header()->setStretchLastSection(true);

    ui->filterScroller->horizontalScrollBar()->setEnabled(false);
    ui->filterScroller->verticalScrollBar()->setEnabled(false);
    ui->filterScroller->setStyleSheet("QScrollArea {border:none;padding:0px;}");

    connect(ui->objectEditTreeView->header(), &QHeaderView::sectionResized, this, &DobObjectEditWidget::OnSectionResized);
    QTimer::singleShot(1, [this]{
        OnSectionResized(0, 0, ui->objectEditTreeView->columnWidth(0));
        OnSectionResized(1, 0, ui->objectEditTreeView->columnWidth(1));
        OnSectionResized(2, 0, ui->objectEditTreeView->columnWidth(2));
        OnSectionResized(3, 0, ui->objectEditTreeView->columnWidth(3));
        OnSectionResized(4, 0, ui->objectEditTreeView->columnWidth(4));
        auto height = ui->nameFilterEdit->height();
        ui->nullFilterCombo->setFixedHeight(height);
        ui->nullFilterCombo->setMaximumHeight(height);
        ui->nullFilterCombo->setMinimumHeight(height);
        ui->changedFilterCombo->setFixedHeight(height);
        ui->changedFilterCombo->setMaximumHeight(height);
        ui->changedFilterCombo->setMinimumHeight(height);
    });

    connect(ui->objectEditTreeView->header(),&QHeaderView::geometriesChanged, this, &DobObjectEditWidget::PositionFilters);
    connect(ui->objectEditTreeView->horizontalScrollBar(), &QAbstractSlider::rangeChanged, this,  &DobObjectEditWidget::PositionFilters);
    connect(ui->objectEditTreeView->horizontalScrollBar(), &QAbstractSlider::actionTriggered, this, &DobObjectEditWidget::PositionFilters);

    // Handle filter changes
    auto placeholder = QString("%1 Filter").arg(QString::fromUtf8("\xF0\x9F\x94\x8D")); // utf-8 Left-Pointing Magnifying Glass
    ui->nameFilterEdit->setPlaceholderText(placeholder);
    ui->valueFilterEdit->setPlaceholderText(placeholder);
    ui->typeFilterEdit->setPlaceholderText(placeholder);

    connect(ui->nameFilterEdit, &QLineEdit::textChanged, this, [this](const QString& f) {ApplyFilter(f, 0, ui->nameFilterEdit); });
    connect(ui->valueFilterEdit, &QLineEdit::textChanged, this, [this](const QString& f) {ApplyFilter(f, 1, ui->valueFilterEdit); });
    connect(ui->nullFilterCombo, QOverload<int>::of(&QComboBox::currentIndexChanged), this, [this](int i){ ApplyFilter(i == 0 ? "" : ui->nullFilterCombo->currentText(), 2, ui->nullFilterCombo); });
    connect(ui->changedFilterCombo, QOverload<int>::of(&QComboBox::currentIndexChanged), this, [this](int i) { ApplyFilter(i == 0 ? "" : ui->changedFilterCombo->currentText(), 3, ui->changedFilterCombo); });
    connect(ui->typeFilterEdit, &QLineEdit::textChanged, this, [this](const QString& f) {ApplyFilter(f, 4, ui->typeFilterEdit); });

    // Disable built in edit triggers and handle it manually below.
    ui->objectEditTreeView->setEditTriggers(QAbstractItemView::NoEditTriggers);

    // Eventfilter that enters edit mode for selected row when pressing return key.
    ui->objectEditTreeView->installEventFilter(this);

    // Make single click enter edit mode.
    connect(ui->objectEditTreeView, &QTreeView::clicked, this, [this](const QModelIndex& index)
    {
        if (!index.isValid())
            return;

        auto p = static_cast<MemberTreeItem*>(index.data(DobObjectModel::InternalDataRole).value<void*>());

        // Array root elements have no edit mode
        if (p->IsContainerRootItem() && p->GetMemberInfo()->collectionType == ArrayCollectionType)
        {
            return;
        }

        // Edit change flag when click column 3, except for Sequences that does not have change flags on individual items.
        auto isSequenceItem = p->GetMemberInfo()->collectionType == SequenceCollectionType && !p->IsContainerRootItem();
        if (index.column() == 3 && !isSequenceItem)
        {
            ui->objectEditTreeView->edit(index);
        }
        else if (index.column() < 3) // Click on name, value, null column opens the ValueEditor
        {
            const bool ctrlKey = qApp->keyboardModifiers().testFlag(Qt::ControlModifier);
            if (!ctrlKey)
            {
                EditValue(index);
            }
            else if (p->GetMemberInfo()->memberType == EntityIdMemberType && !p->IsNull())
            {
                auto eid = ::Utilities::StringToEntityId(p->GetValue());
                if (eid.first)
                {
                    m_dob->ReadEntity(eid.second);
                }
            }
        }
    });

    connect(m_sourceModel, &DobObjectModel::OpenEditor, this, [this](const QModelIndex& srcIndex)
    {
        auto proxyModel = static_cast<ObjectSortFilterProxyModel*>(ui->objectEditTreeView->model());
        auto index = proxyModel->mapFromSource(srcIndex);
        // For some reason, sometimes the node we want to edit will not expand unless we do a collpse-expand before enter edit mode.
        // We also queue up the calls to let any currently open editor close before we open a new one. If not Qt outputs error messages.
        QMetaObject::invokeMethod(qApp, [this, index]{ui->objectEditTreeView->collapse(index.parent());}, Qt::QueuedConnection);
        QMetaObject::invokeMethod(qApp, [this, index]{ui->objectEditTreeView->expand(index.parent());}, Qt::QueuedConnection);
        QMetaObject::invokeMethod(qApp, [this, index]{EditValue(index);}, Qt::QueuedConnection);
    });

    // Expand containers when a child item is added.
    connect(m_sourceModel, &QAbstractItemModel::rowsInserted, this, [this](const QModelIndex &srcParent, int /*first*/, int /*last*/){
        auto proxyModel = static_cast<ObjectSortFilterProxyModel*>(ui->objectEditTreeView->model());
        auto parent = proxyModel->mapFromSource(srcParent);
        ui->objectEditTreeView->expand(parent);
    });

    // Setup context menu
    m_contextMenuHandler = new DobObjectEditContextMenuHandler(ui->objectEditTreeView);
    connect(m_contextMenuHandler, &DobObjectEditContextMenuHandler::SetAllChangeFlags, this, [this](bool changed){ m_sourceModel->SetAllChangeFlags(changed);});

    // Setup operation buttons
    auto dobBaseClass = TypesystemRepository::Instance().GetClass(m_typeId)->dobBaseClass;

    if (dobBaseClass == TypesystemRepository::Entity)
    {
        connect(ui->operationsWidget->liveUpdateCheckBox, &QCheckBox::toggled, this, [this](bool checked)
        {
            if (checked)
            {
                connect(m_dob, &DobHandler::OnEntity, this, &DobObjectEditWidget::OnLiveUpdateEntity);

            }
            else
            {
                m_sourceModel->DisableLiveUpdate();
                disconnect(m_dob, &DobHandler::OnEntity, this, &DobObjectEditWidget::OnLiveUpdateEntity);
            }
        });
    }


    ui->operationsWidget->nameLabel->setText(TypesystemRepository::Instance().GetClass(m_typeId)->name);
    ui->operationsWidget->typeIdLabel->setText(QString::number(m_typeId));
    ui->operationsWidget->SetConfiguration(dobBaseClass);

    connect(ui->operationsWidget->copyNameBtn, &QToolButton::clicked, this, [this]
    {
        qApp->clipboard()->setText(ui->operationsWidget->nameLabel->text());
    });
    connect(ui->operationsWidget->copyTypeIdBtn, &QToolButton::clicked, this, [this]
    {
        qApp->clipboard()->setText(ui->operationsWidget->typeIdLabel->text());
    });
    connect(ui->operationsWidget->copyEntityIdBtn, &QToolButton::clicked, this, [this]
    {
        auto nameStr = ui->operationsWidget->nameLabel->text();
        auto instanceStr = QString::number(ui->operationsWidget->Instance().GetRawValue());
        qApp->clipboard()->setText(QString("%1 : %2").arg(nameStr, instanceStr));
    });

    connect(ui->operationsWidget->setChangesBtn, &QPushButton::clicked, this, [this]
    {
        auto obj = BuildObject();
        auto ent = std::dynamic_pointer_cast<Safir::Dob::Entity>(obj);
        m_dob->SetChanges(ent, ui->operationsWidget->Instance(), ui->operationsWidget->Handler());
    });
    connect(ui->operationsWidget->setAllBtn, &QPushButton::clicked, this,  [this]
    {
        auto obj = BuildObject();
        auto ent = std::dynamic_pointer_cast<Safir::Dob::Entity>(obj);
        m_dob->SetAll(ent, ui->operationsWidget->Instance(), ui->operationsWidget->Handler());
    });
    connect(ui->operationsWidget->deleteBtn, &QPushButton::clicked, this, [this]
    {
        m_dob->Delete(sdt::EntityId(m_typeId, ui->operationsWidget->Instance()), ui->operationsWidget->Handler());
    });
    connect(ui->operationsWidget->createReqBtn, &QPushButton::clicked, this, [this]
    {
        auto obj = BuildObject();
        auto ent = std::dynamic_pointer_cast<Safir::Dob::Entity>(obj);
        m_dob->CreateRequest(ent, ui->operationsWidget->Instance(), ui->operationsWidget->Handler());
    });
    connect(ui->operationsWidget->updateReqBtn, &QPushButton::clicked, this, [this]
    {
        auto obj = BuildObject();
        auto ent = std::dynamic_pointer_cast<Safir::Dob::Entity>(obj);
        m_dob->UpdateRequest(ent, ui->operationsWidget->Instance());
    });
    connect(ui->operationsWidget->deleteReqBtn, &QPushButton::clicked, this, [this]
    {
        m_dob->DeleteRequest(sdt::EntityId(m_typeId, ui->operationsWidget->Instance()));
    });
    connect(ui->operationsWidget->sendMsgBtn, &QPushButton::clicked, this, [this]
    {
        auto obj = BuildObject();
        auto msg = std::dynamic_pointer_cast<Safir::Dob::Message>(obj);
        m_dob->SendMessage(msg, ui->operationsWidget->Channel());
    });
    connect(ui->operationsWidget->sendReqBtn, &QPushButton::clicked, this, [this]
    {
        auto obj = BuildObject();
        auto req = std::dynamic_pointer_cast<Safir::Dob::Service>(obj);
        m_dob->SendServiceRequest(req, ui->operationsWidget->Handler());
    });
    connect(ui->operationsWidget->toXmlBtn, &QPushButton::clicked, this, [this]
    {
        auto obj = BuildObject();
        auto xml = QString::fromStdWString(Safir::Dob::Typesystem::Serialization::ToXml(obj));
        emit XmlSerializedObject(QString::fromStdWString(sdt::Operations::GetName(m_typeId)), xml);
    });
    connect(ui->operationsWidget->toJsonBtn, &QPushButton::clicked, this, [this]
    {
        auto obj = BuildObject();
        auto json = QString::fromStdWString(Safir::Dob::Typesystem::Serialization::ToJson(obj));
        emit JsonSerializedObject(QString::fromStdWString(sdt::Operations::GetName(m_typeId)), json);
    });
}

void DobObjectEditWidget::OnLiveUpdateEntity(const sdt::EntityId& entityId, const sdt::HandlerId&, const Safir::Dob::EntityPtr& entity, DobInterface::EntityOperation operation)
{
    if (entityId.GetTypeId() != m_typeId || ui->operationsWidget->Instance() != entityId.GetInstanceId() || operation == DobInterface::DeletedEntity)
    {
        return;
    }

    // Save the expanded node state
    ExpandedRow state;
    std::function<void(const QModelIndex&, ExpandedRow&)> saveExpandedState;
    saveExpandedState = [this, &saveExpandedState](const QModelIndex &ix, ExpandedRow& expanded)
    {
        if (ix.isValid() && ui->objectEditTreeView->isExpanded(ix))
        {
            ExpandedRow er;
            er.row = ix.row();
            for (auto row = 0; row < ui->objectEditTreeView->model()->rowCount(ix); ++row)
            {
                saveExpandedState(ui->objectEditTreeView->model()->index(row, 0, ix), er);
            }
            expanded.subRows.push_back(er);
        }
    };
    for (auto row = 0; row < ui->objectEditTreeView->model()->rowCount(); ++row)
    {
        saveExpandedState(ui->objectEditTreeView->model()->index(row, 0), state);
    }

    // Update the model
    m_sourceModel->LiveUpdateModel(entity);

    // Reset expanded state
    std::function<void(const QModelIndex&, ExpandedRow&)> resetExpandedState;
    resetExpandedState = [this, &resetExpandedState](const QModelIndex& parent, ExpandedRow& expanded)
    {
        auto ix = ui->objectEditTreeView->model()->index(expanded.row, 0, parent);
        ui->objectEditTreeView->expand(ix);
        for (auto& er : expanded.subRows)
        {
            resetExpandedState(ix, er);
        }
    };
    for (auto& er : state.subRows)
    {
        resetExpandedState({}, er);
    }
}

void DobObjectEditWidget::EditValue(const QModelIndex& index)
{
    if (index.isValid())
    {
        // Click elsewhere on a row will edit value
        auto valIndex = ui->objectEditTreeView->model()->index(index.row(), 1, index.parent());
        ui->objectEditTreeView->setCurrentIndex(valIndex); // This line is needed to make tab open next editor correctly.
        ui->objectEditTreeView->edit(valIndex);
    }
}

bool DobObjectEditWidget::eventFilter(QObject* /*object*/, QEvent *event)
{
    if (event->type() == QEvent::KeyPress)
    {
        QKeyEvent* key = static_cast<QKeyEvent*>(event);
        if (key->key()==Qt::Key_Enter || key->key()==Qt::Key_Return)
        {
            EditValue(ui->objectEditTreeView->currentIndex());
            return true;
        }
    }
    return false;
}

Safir::Dob::Typesystem::ObjectPtr DobObjectEditWidget::BuildObject() const
{
    DobObjectBuilder builder;
    auto obj = builder.CreateObject(m_sourceModel->InvisibleRoot());
    return obj;
}

void DobObjectEditWidget::OnSectionResized(int index, int /*oldSize*/, int newSize)
{
    auto size = newSize - 2;
    switch (index)
    {
    case 0:
        ui->nameFilterEdit->setFixedWidth(size);
        break;
    case 1:
        ui->valueFilterEdit->setFixedWidth(size);
        break;
    case 2:
        ui->nullFilterCombo->setFixedWidth(size);
        break;
    case 3:
        ui->changedFilterCombo->setFixedWidth(size);
        break;
    case 4:
        ui->typeFilterEdit->setFixedWidth(size);
        break;
    }
    PositionFilters();
}

void DobObjectEditWidget::ApplyFilter(const QString& filterText, int column, QWidget* filterWidget)
{
    auto proxyModel = static_cast<ObjectSortFilterProxyModel*>(ui->objectEditTreeView->model());
    if (filterText.isEmpty())
    {
        proxyModel->setFilterRegularExpression(column, QRegularExpression());
    }
    else
    {
        QRegularExpression regex(filterText, QRegularExpression::CaseInsensitiveOption);
        if (regex.isValid())
        {
            proxyModel->setFilterRegularExpression(column, std::move(regex));
        }
        else
        {
            filterWidget->setStyleSheet("background:red;");
            filterWidget->setToolTip(regex.errorString());
            return;
        }
    }

    filterWidget->setStyleSheet("");
    filterWidget->setToolTip("");
}


void DobObjectEditWidget::PositionFilters()
{
    QMetaObject::invokeMethod(this,[this]
        {
            /*
            m_filters.back()->setFixedWidth(std::max(0,m_table->contentsRect().width() -
                                                     m_table->columnViewportPosition(m_table->horizontalHeader()->count() -1) -
                                                     m_table->columnWidth(m_table->horizontalHeader()->count() -1)));
            */
            ui->filterArea->setFixedSize(ui->filterAreaLayout->minimumSize());
            ui->filterScroller->setFixedHeight(ui->filterArea->height());
            ui->filterScroller->horizontalScrollBar()->setSliderPosition(ui->objectEditTreeView->horizontalScrollBar()->sliderPosition());
        },
        Qt::QueuedConnection);
}
