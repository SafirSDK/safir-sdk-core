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
#pragma once

#include <QValidator>
#include <QWidget>
#include <QLineEdit>

class ValueInput : public QWidget
{
    Q_OBJECT
public:
    ValueInput(QWidget* parent) : QWidget(parent) {}
    virtual void SetNull() = 0;
    virtual bool IsNull() const = 0;
    virtual void SetValue(const QString& value) = 0;
    virtual bool IsDelete() const = 0;
    virtual QString GetValue() const = 0;
    virtual void SetValidator(const QValidator* validator) = 0;

signals:
    // Commit data and move to next row. If nextRow=0 just close editor, if 1 move to next row, if -1 move to previous row.
    void Commit(int nextRow);

protected:
    bool eventFilter(QObject *obj, QEvent *event) override;
};

namespace Ui {
class TextValueInput;
class ComboBoxValueInput;
}

class TextValueInput : public ValueInput
{
    Q_OBJECT

public:
    explicit TextValueInput(bool showNullButton, const QString& deleteButtonText, QWidget* parent);
    ~TextValueInput();

    void SetNull() override;
    bool IsNull() const override;
    void SetValue(const QString& value) override;
    bool IsDelete() const override;
    QString GetValue() const override;
    void SetValidator(const QValidator* validator) override;
    void SetMaxLength(int len);

private:
    Ui::TextValueInput *ui;
    bool m_isNull = true;
    bool m_delete = false;
};

class ComboBoxValueInput : public ValueInput
{
    Q_OBJECT

public:
    explicit ComboBoxValueInput(const std::vector<QString>& values, bool showNullItem, const QString& deleteButtonText, QWidget *parent);
    ~ComboBoxValueInput();

    void SetNull() override;
    bool IsNull() const override;
    void SetValue(const QString& value) override;
    bool IsDelete() const override;
    QString GetValue() const override;
    void SetValidator(const QValidator*) override {}

private:
    Ui::ComboBoxValueInput *ui;
    bool m_showNullItem;
    bool m_delete = false;
};
